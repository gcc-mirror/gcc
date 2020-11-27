/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/crate.hpp
 * - AST::Crate type, and other top-level AST definitions
 */
#pragma once

#include "ast.hpp"
#include "types.hpp"
#include <hir/crate_ptr.hpp>

namespace AST {


class ExternCrate;

class TestDesc
{
public:
    ::AST::Path path;
    ::std::string   name;
    bool    ignore = false;
    bool    is_benchmark = false;

    enum class ShouldPanic {
        No,
        Yes,
        YesWithMessage,
    } panic_type = ShouldPanic::No;

    ::std::string   expected_panic_message;
};

class ProcMacroDef
{
public:
    ::std::string   name;
    ::AST::Path path;
    ::std::vector<::std::string>    attributes;
};

class Crate
{
public:
    ::AST::AttributeList    m_attrs;

    ::std::map< ::std::string, ::AST::Path> m_lang_items;
public:
    Module  m_root_module;
    ::std::map< ::std::string, ExternCrate> m_extern_crates;
    // Mapping filled by searching for (?visible) macros with is_pub=true
    ::std::map< ::std::string, const MacroRules*> m_exported_macros;

    // List of tests (populated in expand if --test is passed)
    bool    m_test_harness = false;
    ::std::vector<TestDesc>   m_tests;

    //::std::vector<::std::string>    m_extra_files;

    // Procedural macros!
    ::std::vector<ProcMacroDef> m_proc_macros;

    enum class Type {
        Unknown,
        RustLib,
        RustDylib,
        CDylib,
        Executable,
        ProcMacro,   // Procedural macro
    } m_crate_type = Type::Unknown;
    enum LoadStd {
        LOAD_STD,
        LOAD_CORE,
        LOAD_NONE,
    } m_load_std = LOAD_STD;
    ::std::string   m_crate_name_suffix;
    ::std::string   m_crate_name;
    AST::Path   m_prelude_path;


    Crate();

    const Module& root_module() const { return m_root_module; }
          Module& root_module()       { return m_root_module; }

    /// Load referenced crates
    void load_externs();

    /// Load the named crate and returns the crate's unique name
    /// If the parameter `file` is non-empty, only that particular filename will be loaded (from any of the search paths)
    ::std::string load_extern_crate(Span sp, const ::std::string& name, const ::std::string& file="");
};

/// Representation of an imported crate
class ExternCrate
{
public:
    ::std::string   m_name;
    ::std::string   m_filename;
    ::HIR::CratePtr m_hir;

    ExternCrate(const ::std::string& name, const ::std::string& path);

    ExternCrate(ExternCrate&&) = default;
    ExternCrate& operator=(ExternCrate&&) = default;
    ExternCrate(const ExternCrate&) = delete;
    ExternCrate& operator=(const ExternCrate& ) = delete;

    void with_all_macros(::std::function<void(const ::std::string& , const MacroRules&)> cb) const;
    const MacroRules* find_macro_rules(const ::std::string& name) const;
};

extern ::std::vector<::std::string>    g_crate_load_dirs;
extern ::std::map<::std::string, ::std::string>    g_crate_overrides;

}   // namespace AST
