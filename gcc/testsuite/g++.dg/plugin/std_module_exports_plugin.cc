#include "gcc-plugin.h"
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "intl.h"
#include "cp/cp-tree.h"
#include "cp/name-lookup.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "attribs.h"

int plugin_is_GPL_compatible;

void plugin_dump_ns (tree, char *, hash_set<tree> **);

enum class whitelist_std {
  cxx20 = 1 << 0,
  cxx23 = 1 << 1,
  cxx26 = 1 << 2,
  cxx29 = 1 << 3,
  all = (1 << 4) - 1
};
struct {
  const char *name;
  whitelist_std enabled_in;
} whitelist[] = {
  // [zombie.names] in C++20-C++26, supported as an extension.
  { "std::result_of", whitelist_std::all },
  { "std::result_of_t", whitelist_std::all },
  // [zombie.names] in C++20-C++26, supported in C++20 as an extension.
  { "std::unexpected_handler", whitelist_std::cxx20 },
  // [zombie.names] in C++23-C++26, not exported in C++20.
  { "std::declare_no_pointers", whitelist_std::cxx20 },
  { "std::declare_reachable", whitelist_std::cxx20 },
  { "std::get_pointer_safety", whitelist_std::cxx20 },
  { "std::pointer_safety", whitelist_std::cxx20 },
  { "std::undeclare_no_pointers", whitelist_std::cxx20 },
  { "std::undeclare_reachable", whitelist_std::cxx20 }
};

void
plugin_dump_decl (tree decl, char *scope, hash_set<tree> **exported_usings)
{
  if (VAR_P (decl) && DECL_ARTIFICIAL (decl))
    return;

  tree name = DECL_NAME (decl);
  if (!name)
    return;

  if (IDENTIFIER_ANON_P (name))
    return;

  if (TREE_CODE (decl) == CONST_DECL)
    return;

  if (TREE_CODE (decl) == NAMESPACE_DECL && DECL_NAMESPACE_INLINE_P (decl))
    {
      plugin_dump_ns (decl, scope, exported_usings);
      return;
    }

  if (IDENTIFIER_POINTER (name)[0] == '_'
      || strchr (IDENTIFIER_POINTER (name), ' '))
    return;

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    {
      char *p = strchr (scope, '\0');
      strcpy (p, IDENTIFIER_POINTER (name));
      strcat (p, "::");
      plugin_dump_ns (decl, scope, exported_usings);
      *p = '\0';
      return;
    }

  if (DECL_MODULE_EXPORT_P (decl))
    return;
  if (TREE_DEPRECATED (decl)
      || lookup_attribute ("deprecated", DECL_ATTRIBUTES (decl)))
    return;
  if (TREE_CODE (decl) == TEMPLATE_DECL
      && DECL_TEMPLATE_RESULT (decl)
      && (TREE_DEPRECATED (DECL_TEMPLATE_RESULT (decl))
	  || lookup_attribute ("deprecated",
			       DECL_ATTRIBUTES (DECL_TEMPLATE_RESULT (decl)))))
    return;
  if (*exported_usings
      && (*exported_usings)->contains (DECL_NAME (decl)))
    return;

  size_t scope_len = strlen (scope);
  whitelist_std this_std;
  if (cxx_dialect == cxx20)
    this_std = whitelist_std::cxx20;
  else if (cxx_dialect == cxx23)
    this_std = whitelist_std::cxx23;
  else if (cxx_dialect == cxx26)
    this_std = whitelist_std::cxx26;
  else if (cxx_dialect == cxx29)
    this_std = whitelist_std::cxx29;
  for (int i = 0; i < ARRAY_SIZE (whitelist); ++i)
    if (strncmp (whitelist[i].name, scope, scope_len) == 0
	&& strcmp (whitelist[i].name + scope_len,
		   IDENTIFIER_POINTER (name)) == 0)
      {
	if (((int) whitelist[i].enabled_in & (int) this_std) != 0)
	  {
	    inform (DECL_SOURCE_LOCATION (decl),
		    "missing %<using %s%D;%> whitelisted", scope, name);
	    return;
	  }
	break;
      }

  auto_diagnostic_group d;
  error_at (DECL_SOURCE_LOCATION (decl),
	    "missing %<using %s%D;%> in libstdc++-v3/src/c++23/std*.cc.in",
	    scope, name);
  inform (DECL_SOURCE_LOCATION (decl),
	  "%<%s%D%> found in std namespace but not exported from std module",
	  scope, name);
}

void
plugin_dump_binding (tree binding, char *scope,
		     hash_set<tree> **exported_usings)
{
  tree value = NULL_TREE;

  if (TREE_CODE (binding) == OVERLOAD)
    {
      tree ovl = ovl_skip_hidden (binding);
      if (ovl && TREE_CODE (ovl) == OVERLOAD && OVL_EXPORT_P (ovl))
	return;
    }

  if (STAT_HACK_P (binding))
    {
      if (!STAT_TYPE_HIDDEN_P (binding)
          && STAT_TYPE (binding))
        return plugin_dump_decl (STAT_TYPE (binding), scope, exported_usings);
      else if (!STAT_DECL_HIDDEN_P (binding))
        value = STAT_DECL (binding);
    }
  else
    value = binding;

  value = ovl_skip_hidden (value);
  if (value)
    {
      value = OVL_FIRST (value);
      return plugin_dump_decl (value, scope, exported_usings);
    }
}

void
plugin_dump_ns (tree ns, char *scope, hash_set<tree> **exported_usings)
{
  using itert = hash_table<named_decl_hash>::iterator;
  itert end (DECL_NAMESPACE_BINDINGS (ns)->end ());
  hash_set<tree> *my_exported_usings = NULL;
  if (DECL_NAMESPACE_INLINE_P (ns))
    {
      if (*exported_usings == NULL)
	{
	  *exported_usings = new hash_set<tree>;
	  tree parent = DECL_CONTEXT (ns);
	  itert pend (DECL_NAMESPACE_BINDINGS (parent)->end ());
	  for (itert iter (DECL_NAMESPACE_BINDINGS (parent)->begin ());
	       iter != pend; ++iter)
	    {
	      tree b = *iter;
	      if (TREE_CODE (b) == USING_DECL
		  && DECL_MODULE_EXPORT_P (b)
		  && DECL_NAME (b))
		(*exported_usings)->add (DECL_NAME (b));
	      else if (TREE_CODE (b) == OVERLOAD)
		{
		  tree ovl = ovl_skip_hidden (b);
		  if (ovl && TREE_CODE (ovl) == OVERLOAD && OVL_EXPORT_P (ovl))
		    {
		      b = OVL_FIRST (ovl);
		      if (DECL_NAME (b))
			(*exported_usings)->add (DECL_NAME (b));
		    }
		}
	    }
	}
    }
  else
    exported_usings = &my_exported_usings;
  for (itert iter (DECL_NAMESPACE_BINDINGS (ns)->begin ());
       iter != end; ++iter)
    {
      tree b = *iter;
      gcc_assert (TREE_CODE (b) != BINDING_VECTOR);
      plugin_dump_binding (b, scope, exported_usings);
    }
  delete my_exported_usings;
}

void
plugin_finish_unit (void *, void *)
{
  if (!main_input_filename
      || strstr (main_input_filename, "/std.cc") == NULL)
    return;

  char buf[4096];
  strcpy (buf, "std::");
  hash_set<tree> *exported_usings = NULL;
  plugin_dump_ns (std_node, buf, &exported_usings);
  delete exported_usings;
}

int
plugin_init (struct plugin_name_args *plugin_info,
             struct plugin_gcc_version *version)
{
  const char *plugin_name = plugin_info->base_name;

  register_callback (plugin_name, PLUGIN_FINISH_UNIT,
                     plugin_finish_unit, NULL);
  return 0;
}
