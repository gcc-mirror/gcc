/* Subroutines used for parsing target attribute for RISC-V.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tm_p.h"
#include "diagnostic.h"
#include "opts.h"
#include "riscv-subset.h"

namespace {
class riscv_target_attr_parser
{
public:
  riscv_target_attr_parser (location_t loc)
    : m_found_arch_p (false)
    , m_found_tune_p (false)
    , m_found_cpu_p (false)
    , m_found_priority_p (false)
    , m_subset_list (nullptr)
    , m_loc (loc)
    , m_cpu_info (nullptr)
    , m_tune (nullptr)
    , m_priority (0)
  {
  }

  bool handle_arch (const char *);
  bool handle_cpu (const char *);
  bool handle_tune (const char *);
  bool handle_priority (const char *);

  void update_settings (struct gcc_options *opts) const;
private:
  const char *m_raw_attr_str;
  bool parse_arch (const char *);

  bool m_found_arch_p;
  bool m_found_tune_p;
  bool m_found_cpu_p;
  bool m_found_priority_p;
  riscv_subset_list *m_subset_list;
  location_t m_loc;
  const  riscv_cpu_info *m_cpu_info;
  const char *m_tune;
  int m_priority;
};
}

/* All the information needed to handle a target attribute.
   NAME is the name of the attribute.
   HANDLER is the function that takes the attribute string as an argument.  */

struct riscv_attribute_info
{
  const char *name;
  bool (riscv_target_attr_parser::*handler) (const char *);
};

/* The target attributes that we support.  */

static const struct riscv_attribute_info riscv_target_attrs[]
  = {{"arch", &riscv_target_attr_parser::handle_arch},
     {"cpu", &riscv_target_attr_parser::handle_cpu},
     {"tune", &riscv_target_attr_parser::handle_tune},
     {NULL, NULL}};

static const struct riscv_attribute_info riscv_target_version_attrs[]
  = {{"arch", &riscv_target_attr_parser::handle_arch},
     {"priority", &riscv_target_attr_parser::handle_priority},
     {NULL, NULL}};

bool
riscv_target_attr_parser::parse_arch (const char *str)
{
  if (m_subset_list)
    delete m_subset_list;
  /* Check if it's setting full arch string.  */
  if (strncmp ("rv", str, strlen ("rv")) == 0)
    {
      if (TARGET_64BIT && strncmp ("32", str + 2, strlen ("32")) == 0)
	{
	  error_at (m_loc, "unexpected arch for %<target()%> attribute: "
		    "must start with rv64 but found %qs", str);
	  goto fail;
	}

      if (!TARGET_64BIT && strncmp ("64", str + 2, strlen ("64")) == 0)
	{
	  error_at (m_loc, "unexpected arch for %<target()%> attribute: "
		    "must start with rv32 but found %qs", str);
	  goto fail;
	}

      m_subset_list = riscv_subset_list::parse (str, m_loc);

      if (m_subset_list == nullptr)
	goto fail;

      return true;
    }
  else
    {
      /* Parsing the extension list like "+<ext>[,+<ext>]*".  */
      size_t len = strlen (str);
      std::unique_ptr<char[]> buf (new char[len+1]);
      char *str_to_check = buf.get ();
      strcpy (str_to_check, str);
      const char *token = strtok_r (str_to_check, ",", &str_to_check);
      const char *local_arch_str = global_options.x_riscv_arch_string;
      m_subset_list = local_arch_str
		      ? riscv_subset_list::parse (local_arch_str, m_loc)
		      : riscv_cmdline_subset_list ()->clone ();
      m_subset_list->set_loc (m_loc);
      m_subset_list->set_allow_adding_dup (true);

      while (token)
	{
	  if (token[0] != '+')
	    {
	      error_at (
		m_loc,
		"unexpected arch for %<target()%> attribute: must start "
		"with + or rv");
	      goto fail;
	    }

	  const char *result = m_subset_list->parse_single_ext (token + 1);
	  /* Check parse_single_ext has consume all string.  */
	  if (*result != '\0')
	    {
	      error_at (
		m_loc,
		"unexpected arch for %<target()%> attribute: bad "
		"string found %qs", token);
	      goto fail;
	    }

	  token = strtok_r (NULL, ",", &str_to_check);
	}

      m_subset_list->set_allow_adding_dup (false);
      m_subset_list->finalize ();
      return true;
    }
fail:
  if (m_subset_list != nullptr)
    {
      delete m_subset_list;
      m_subset_list = nullptr;
    }
  return false;
}

/* Handle the ARCH_STR argument to the arch= target attribute.  */

bool
riscv_target_attr_parser::handle_arch (const char *str)
{
  if (m_found_arch_p)
    error_at (m_loc, "%<target()%> attribute: arch appears more than once");
  m_found_arch_p = true;
  return parse_arch (str);
}

/* Handle the CPU_STR argument to the cpu= target attribute.  */

bool
riscv_target_attr_parser::handle_cpu (const char *str)
{
  if (m_found_cpu_p)
    error_at (m_loc, "%<target()%> attribute: cpu appears more than once");

  m_found_cpu_p = true;
  const riscv_cpu_info *cpu_info = riscv_find_cpu (str);

  if (!cpu_info)
    {
      error_at (m_loc, "%<target()%> attribute: unknown CPU %qs", str);
      return false;
    }

  if (m_subset_list == nullptr)
    {
      const char *arch_str = cpu_info->arch;
      m_subset_list = riscv_subset_list::parse (arch_str, m_loc);
      gcc_assert (m_subset_list);
    }

  m_cpu_info = cpu_info;
  return true;
}

/* Handle the TUNE_STR argument to the tune= target attribute.  */

bool
riscv_target_attr_parser::handle_tune (const char *str)
{
  if (m_found_tune_p)
    error_at (m_loc, "%<target()%> attribute: tune appears more than once");
  m_found_tune_p = true;
  const struct riscv_tune_info *tune = riscv_parse_tune (str, true);

  if (tune == nullptr)
    {
      error_at (m_loc, "%<target()%> attribute: unknown TUNE %qs", str);
      return false;
    }

  m_tune = tune->name;

  return true;
}

bool
riscv_target_attr_parser::handle_priority (const char *str)
{
  if (m_found_priority_p)
    error_at (m_loc, "%<target()%> attribute: priority appears more than once");
  m_found_priority_p = true;

  if (sscanf (str, "%d", &m_priority) != 1)
    {
      error_at (m_loc, "%<target()%> attribute: invalid priority %qs", str);
      return false;
    }

  return true;
}

void
riscv_target_attr_parser::update_settings (struct gcc_options *opts) const
{
  if (m_subset_list)
    {
      std::string local_arch = m_subset_list->to_string (true);
      const char* local_arch_str = local_arch.c_str ();
      struct cl_target_option *default_opts
	= TREE_TARGET_OPTION (target_option_default_node);
      if (opts->x_riscv_arch_string != default_opts->x_riscv_arch_string)
	free (CONST_CAST (void *, (const void *) opts->x_riscv_arch_string));
      opts->x_riscv_arch_string = xstrdup (local_arch_str);

      riscv_set_arch_by_subset_list (m_subset_list, opts);
    }

  if (m_cpu_info)
    opts->x_riscv_cpu_string = m_cpu_info->name;

  if (m_tune)
    opts->x_riscv_tune_string = m_tune;
  else
    {
      if (m_cpu_info)
	opts->x_riscv_tune_string = m_cpu_info->tune;
    }

  if (m_priority)
    opts->x_riscv_fmv_priority = m_priority;
}

/* Parse ARG_STR which contains the definition of one target attribute.
   Show appropriate errors if any or return true if the attribute is valid.  */

static bool
riscv_process_one_target_attr (char *arg_str,
			       location_t loc,
			       riscv_target_attr_parser &attr_parser,
			       const struct riscv_attribute_info *attrs)
{
  size_t len = strlen (arg_str);

  if (len == 0)
    {
      error_at (loc, "malformed %<target()%> attribute");
      return false;
    }

  std::unique_ptr<char[]> buf (new char[len+1]);
  char *str_to_check = buf.get();
  strcpy (str_to_check, arg_str);

  char *arg = strchr (str_to_check, '=');

  if (!arg)
    {
      error_at (
	loc,
	"attribute %<target(\"%s\")%> does not accept an argument",
	str_to_check);
      return false;
    }

  arg[0] = '\0';
  ++arg;
  for (const auto *attr = attrs;
       attr->name;
       ++attr)
    {
      /* If the names don't match up, or the user has given an argument
	 to an attribute that doesn't accept one, or didn't give an argument
	 to an attribute that expects one, fail to match.  */
      if (strncmp (str_to_check, attr->name, strlen (attr->name)) != 0)
	continue;

      return (&attr_parser->*attr->handler) (arg);
    }

  error_at (loc, "Got unknown attribute %<target(\"%s\")%>", str_to_check);
  return false;
}

/* Count how many times the character C appears in
   NULL-terminated string STR.  */

static unsigned int
num_occurrences_in_str (char c, char *str)
{
  unsigned int res = 0;
  while (*str != '\0')
    {
      if (*str == c)
	res++;

      str++;
    }

  return res;
}

/* Parse the string in ARGS that contains the target attribute information
   and update the global target options space.  */

bool
riscv_process_target_attr (const char *args,
			   location_t loc,
			   const struct riscv_attribute_info *attrs)
{
  size_t len = strlen (args);

  /* No need to emit warning or error on empty string here, generic code already
     handle this case.  */
  if (len == 0)
    {
      return false;
    }

  std::unique_ptr<char[]> buf (new char[len+1]);
  char *str_to_check = buf.get ();
  strcpy (str_to_check, args);

  /* Used to catch empty spaces between semi-colons i.e.
     attribute ((target ("attr1;;attr2"))).  */
  unsigned int num_semicolons = num_occurrences_in_str (';', str_to_check);

  /* Handle multiple target attributes separated by ';'.  */
  char *token = strtok_r (str_to_check, ";", &str_to_check);

  riscv_target_attr_parser attr_parser (loc);
  unsigned int num_attrs = 0;
  while (token)
    {
      num_attrs++;
      if (!riscv_process_one_target_attr (token, loc, attr_parser, attrs))
	return false;

      token = strtok_r (NULL, ";", &str_to_check);
    }

  if (num_attrs != num_semicolons + 1)
    {
      error_at (loc, "malformed %<target(\"%s\")%> attribute",
		args);
      return false;
    }

  /* Apply settings from target attribute.  */
  attr_parser.update_settings (&global_options);

  return true;
}

/* Parse the tree in ARGS that contains the target attribute information
   and update the global target options space.  */

static bool
riscv_process_target_attr (tree args,
			   location_t loc,
			   const struct riscv_attribute_info *attrs)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree head = TREE_VALUE (args);
	  if (head)
	    {
	      if (!riscv_process_target_attr (head, loc, attrs))
		return false;
	    }
	  args = TREE_CHAIN (args);
      } while (args);

      return true;
    }

  if (TREE_CODE (args) != STRING_CST)
    {
      error_at (loc, "attribute %<target%> argument not a string");
      return false;
    }

  return riscv_process_target_attr (TREE_STRING_POINTER (args), loc, attrs);
}

/* Implement TARGET_OPTION_VALID_ATTRIBUTE_P.
   This is used to process attribute ((target ("..."))).
   Note, that riscv_set_current_function() has not been called before,
   so we need must not mess with the current global_options, which
   likely belong to another function.  */

bool
riscv_option_valid_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree new_target;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  location_t loc = DECL_SOURCE_LOCATION (fndecl);

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);

  /* If fndecl already has some target attributes applied to it, unpack
     them so that we add this attribute on top of them, rather than
     overwriting them.  */
  if (existing_target)
    {
      struct cl_target_option *existing_options
	= TREE_TARGET_OPTION (existing_target);

      if (existing_options)
	cl_target_option_restore (&global_options, &global_options_set,
				  existing_options);
    }
  else
    cl_target_option_restore (&global_options, &global_options_set,
			      TREE_TARGET_OPTION (target_option_default_node));

  /* Now we can parse the attributes and set &global_options accordingly.  */
  ret = riscv_process_target_attr (args, loc, riscv_target_attrs);
  if (ret)
    {
      riscv_override_options_internal (&global_options);
      new_target = build_target_option_node (&global_options,
					     &global_options_set);
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;
    }

  /* Restore current target options to original state.  */
  cl_target_option_restore (&global_options, &global_options_set, &cur_target);
  return ret;
}

/* Parse the tree in ARGS that contains the target_version attribute
   information and update the global target options space.  */

bool
riscv_process_target_version_attr (tree args, location_t loc)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      if (TREE_CHAIN (args))
	{
	  error ("attribute %<target_version%> has multiple values");
	  return false;
	}
      args = TREE_VALUE (args);
    }

  if (!args || TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target_version%> argument not a string");
      return false;
    }

  const char *str = TREE_STRING_POINTER (args);
  if (strcmp (str, "default") == 0)
    return true;

  return riscv_process_target_attr (str, loc, riscv_target_version_attrs);
}


/* Implement TARGET_OPTION_VALID_VERSION_ATTRIBUTE_P.  This is used to
   process attribute ((target_version ("..."))).  */

bool
riscv_option_valid_version_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree new_target;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  location_t loc = DECL_SOURCE_LOCATION (fndecl);

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);

  /* If fndecl already has some target attributes applied to it, unpack
     them so that we add this attribute on top of them, rather than
     overwriting them.  */
  if (existing_target)
    {
      struct cl_target_option *existing_options
	= TREE_TARGET_OPTION (existing_target);

      if (existing_options)
	cl_target_option_restore (&global_options, &global_options_set,
				  existing_options);
    }
  else
    cl_target_option_restore (&global_options, &global_options_set,
			      TREE_TARGET_OPTION (target_option_current_node));

  ret = riscv_process_target_version_attr (args, loc);

  /* Set up any additional state.  */
  if (ret)
    {
      riscv_override_options_internal (&global_options);
      new_target = build_target_option_node (&global_options,
					     &global_options_set);
    }
  else
    new_target = NULL;

  if (fndecl && ret)
    DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);

  return ret;
}
