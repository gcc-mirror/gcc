/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/crate.cpp
 * - Helper functions for the AST::Crate type (includes loading `extern crate`s)
 */
#include "crate.hpp"
#include "ast.hpp"
#include "../parse/parseerror.hpp"
#include "../expand/cfg.hpp"
#include <hir/hir.hpp>  // HIR::Crate
#include <hir/main_bindings.hpp>    // HIR_Deserialise
#include <fstream>

::std::vector<::std::string>    AST::g_crate_load_dirs = { };
::std::map<::std::string, ::std::string>    AST::g_crate_overrides;

namespace {
    bool check_item_cfg(const ::AST::AttributeList& attrs)
    {
        for(const auto& at : attrs.m_items) {
            if( at.name() == "cfg" && !check_cfg(at.span(), at) ) {
                return false;
            }
        }
        return true;
    }
    void iterate_module(::AST::Module& mod, ::std::function<void(::AST::Module& mod)> fcn)
    {
        fcn(mod);
        for( auto& sm : mod.items() )
        {
            TU_IFLET(::AST::Item, sm.data, Module, e,
                if( check_item_cfg(sm.data.attrs) )
                {
                    iterate_module(e, fcn);
                }
            )
        }
        // TODO: What about if an anon mod has been #[cfg]-d out?
        // - For now, disable
        //for(const auto& anon : mod.anon_mods() ) {
        //    iterate_module(*anon, fcn);
        //}
    }
}


namespace AST {

Crate::Crate():
    m_root_module(::AST::Path("",{})),
    m_load_std(LOAD_STD)
{
}

void Crate::load_externs()
{
    auto cb = [this](Module& mod) {
        for( /*const*/ auto& it : mod.items() )
        {
            TU_IFLET(AST::Item, it.data, Crate, c,
                if( check_item_cfg(it.data.attrs) )
                {
                    c.name = load_extern_crate( it.data.span, c.name );
                }
            )
        }
        };
    iterate_module(m_root_module, cb);

    // Check for no_std or no_core, and load libstd/libcore
    // - Duplicates some of the logic in "Expand", but also helps keep crate loading separate to most of expand
    // NOTE: Not all crates are loaded here, any crates loaded by macro invocations will be done during expand.
    bool no_std  = false;
    bool no_core = false;

    for( const auto& a : this->m_attrs.m_items )
    {
        if( a.name() == "no_std" )
            no_std = true;
        if( a.name() == "no_core" )
            no_core = true;
        if( a.name() == "cfg_attr" && a.items().size() == 2 ) {
            if( check_cfg(a.span(), a.items().at(0)) )
            {
                const auto& a2 = a.items().at(1);
                if( a2.name() == "no_std" )
                    no_std = true;
                if( a2.name() == "no_core" )
                    no_core = true;
            }
        }
    }

    if( no_core ) {
        // Don't load anything
    }
    else if( no_std ) {
        auto n = this->load_extern_crate(Span(), "core");
        ASSERT_BUG(Span(), n == "core", "libcore wasn't loaded as `core`, instead `" << n << "`");
    }
    else {
        auto n = this->load_extern_crate(Span(), "std");
        ASSERT_BUG(Span(), n == "std", "libstd wasn't loaded as `std`, instead `" << n << "`");
    }
}
// TODO: Handle disambiguating crates with the same name (e.g. libc in std and crates.io libc)
// - Crates recorded in rlibs should specify a hash/tag that's passed in to this function.
::std::string Crate::load_extern_crate(Span sp, const ::std::string& name, const ::std::string& basename/*=""*/)
{
    DEBUG("Loading crate '" << name << "'");

    ::std::string   path;
    auto it = g_crate_overrides.find(name);
    if(basename == "" && it != g_crate_overrides.end())
    {
        path = it->second;
        if( !::std::ifstream(path).good() ) {
            ERROR(sp, E0000, "Unable to open crate '" << name << "' at path " << path);
        }
    }
    else
    {
        // Search a list of load paths for the crate
        for(const auto& p : g_crate_load_dirs)
        {
            if( basename == "" )
            {
                path = p + "/lib" + name + ".hir";
                // TODO: Search for `p+"/lib"+name+"-*.hir" (which would match e.g. libnum-0.11.hir)
            }
            else
            {
                path = p + "/" + basename;
            }

            if( ::std::ifstream(path).good() ) {
                break ;
            }
        }
        if( !::std::ifstream(path).good() ) {
            if( basename.empty() )
                ERROR(sp, E0000, "Unable to locate crate '" << name << "' in search directories");
            else
                ERROR(sp, E0000, "Unable to locate crate '" << name << "' with filename " << basename << " in search directories");
        }
    }

    // NOTE: Creating `ExternCrate` loads the crate from the specified path
    auto ec = ExternCrate { name, path };
    auto real_name = ec.m_hir->m_crate_name;
    assert(!real_name.empty());
    auto res = m_extern_crates.insert(::std::make_pair( real_name, mv$(ec) ));
    if( !res.second ) {
        // Crate already loaded?
    }
    auto& ext_crate = res.first->second;
    // Move the external list out (doesn't need to be kept in the nested crate)
    auto crate_ext_list = mv$( ext_crate.m_hir->m_ext_crates );

    // Load referenced crates
    for( const auto& ext : crate_ext_list )
    {
        if( m_extern_crates.count(ext.first) == 0 )
        {
            const auto load_name = this->load_extern_crate(sp, ext.first, ext.second.m_basename);
            if( load_name != ext.first )
            {
                // ERROR - The crate loaded wasn't the one that was used when compiling this crate.
                ERROR(sp, E0000, "The crate file `" << ext.second.m_basename << "` didn't load the expected crate - have " << load_name << " != exp " << ext.first);
            }
        }
    }

    DEBUG("Loaded '" << name << "' from '" << basename << "' (actual name is '" << real_name << "')");
    return real_name;
}

ExternCrate::ExternCrate(const ::std::string& name, const ::std::string& path):
    m_name(name),
    m_filename(path)
{
    TRACE_FUNCTION_F("name=" << name << ", path='" << path << "'");
    m_hir = HIR_Deserialise(path, name);

    m_hir->post_load_update(name);
    m_name = m_hir->m_crate_name;
}

void ExternCrate::with_all_macros(::std::function<void(const ::std::string& , const MacroRules&)> cb) const
{
    for(const auto& m : m_hir->m_exported_macros)
    {
        cb(m.first, *m.second);
    }
}
const MacroRules* ExternCrate::find_macro_rules(const ::std::string& name) const
{
    auto i = m_hir->m_exported_macros.find(name);
    if(i != m_hir->m_exported_macros.end())
        return &*i->second;
    return nullptr;
}


}   // namespace AST

