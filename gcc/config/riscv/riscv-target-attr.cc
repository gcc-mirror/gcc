/* Subroutines used for parsing target attribute for RISC-V.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
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
    , m_subset_list (nullptr)
    , m_loc (loc)
    , m_cpu_info (nullptr)
    , m_tune (nullptr)
  {
  }

  bool handle_arch (const char *);
  bool handle_cpu (const char *);
  bool handle_tune (const char *);

  void set_loc (location_t loc) {
    m_loc = loc;
  }

  void update_settings (struct gcc_options *opts) const;
private:
  const char *m_raw_attr_str;
  bool parse_arch (const char *);

  bool m_found_arch_p;
  bool m_found_tune_p;
  bool m_found_cpu_p;
  riscv_subset_list *m_subset_list;
  location_t m_loc;
  const  riscv_cpu_info *m_cpu_info;
  const char *m_tune;
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

static const struct riscv_attribute_info riscv_attributes[]
  = {{"arch", &riscv_target_attr_parser::handle_arch},
     {"cpu", &riscv_target_attr_parser::handle_cpu},
     {"tune", &riscv_target_attr_parser::handle_tune}};

bool
riscv_target_attr_parser::parse_arch (const char *str)
{
  if (m_subset_list)
    delete m_subset_list;
  /* Check if it's setting full arch string.  */
  if (strncmp ("rv", str, strlen ("rv")) == 0)
    {
      m_subset_list = riscv_subset_list::parse (str, location_t ());

      if (m_subset_list == nullptr)
	goto fail;

      return true;
    }
  else
    {
      /* Parsing the extension list like "+<ext>[,+<ext>]*".  */
      size_t len = strlen (str);
      std::unique_ptr<char[]> buf (new char[len]);
      char *str_to_check = buf.get ();
      strcpy (str_to_check, str);
      const char *token = strtok_r (str_to_check, ",", &str_to_check);
      m_subset_list = riscv_current_subset_list ()->clone ();
      m_subset_list->set_loc (m_loc);
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
	  else
	    {
	      const char *result = m_subset_list->parse_single_ext (token + 1);
	      /* Check parse_single_ext has consume all string.  */
	      if (*result != '\0')
		{
		  error_at (
		    m_loc,
		    "unexpected arch for %<target()%> attribute: bad "
		    "string found %<%s%>", token);
		  goto fail;
		}
	    }
	  token = strtok_r (NULL, ",", &str_to_check);
	}
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

void
riscv_target_attr_parser::update_settings (struct gcc_options *opts) const
{
  if (m_subset_list)
    riscv_set_arch_by_subset_list (m_subset_list, opts);

  if (m_cpu_info)
    opts->x_riscv_cpu_string = m_cpu_info->name;

  if (m_tune)
    opts->x_riscv_tune_string = m_tune;
  else
    {
      if (m_cpu_info)
	opts->x_riscv_tune_string = m_cpu_info->tune;
    }
}

/* Parse ARG_STR which contains the definition of one target attribute.
   Show appropriate errors if any or return true if the attribute is valid.  */

static bool
riscv_process_one_target_attr (char *arg_str,
			       location_t loc,
			       riscv_target_attr_parser &attr_parser)
{
  size_t len = strlen (arg_str);

  if (len == 0)
    {
      error_at (loc, "malformed %<target()%> attribute");
      return false;
    }

  std::unique_ptr<char[]> buf (new char[len]);
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
  for (const auto &attr : riscv_attributes)
    {
      /* If the names don't match up, or the user has given an argument
	 to an attribute that doesn't accept one, or didn't give an argument
	 to an attribute that expects one, fail to match.  */
      if (strncmp (str_to_check, attr.name, strlen (attr.name)) != 0)
	continue;

      return (&attr_parser->*attr.handler) (arg);
    }
  error_at (loc, "Got unknown attribute %<target(\"%s\")%>", str_to_check);

  return false;
}

/* Count how many times the character C appears in
   NULL-terminated string STR.  */

static unsigned int
num_occurences_in_str (char c, char *str)
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

/* Parse the tree in ARGS that contains the target attribute information
   and update the global target options space.  */

static bool
riscv_process_target_attr (tree args, location_t loc, struct gcc_options *opts)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree head = TREE_VALUE (args);
	  if (head)
	    {
	      if (!riscv_process_target_attr (head, loc, opts))
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
  size_t len = strlen (TREE_STRING_POINTER (args));

  /* No need to emit warning or error on empty string here, generic code already
     handle this case.  */
  if (len == 0)
    {
      return false;
    }

  std::unique_ptr<char[]> buf (new char[len]);
  char *str_to_check = buf.get ();
  strcpy (str_to_check, TREE_STRING_POINTER (args));

  /* Used to catch empty spaces between commas i.e.
     attribute ((target ("attr1;;attr2"))).  */
  unsigned int num_commas = num_occurences_in_str (';', str_to_check);

  /* Handle multiple target attributes separated by ','.  */
  char *token = strtok_r (str_to_check, ";", &str_to_check);

  riscv_target_attr_parser attr_parser (loc);
  unsigned int num_attrs = 0;
  while (token)
    {
      num_attrs++;
      riscv_process_one_target_attr (token, loc, attr_parser);
      token = strtok_r (NULL, ";", &str_to_check);
    }

  if (num_attrs != num_commas + 1)
    {
      error_at (loc, "malformed %<target(\"%s\")%> attribute",
		TREE_STRING_POINTER (args));
      return false;
    }

  /* Apply settings from target attribute.  */
  attr_parser.update_settings (opts);

  return true;
}

/* Implement TARGET_OPTION_VALID_ATTRIBUTE_P.  This is used to
   process attribute ((target ("..."))).  */

bool
riscv_option_valid_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree new_target;
  location_t loc = DECL_SOURCE_LOCATION (fndecl);

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);

  ret = riscv_process_target_attr (args, loc, &global_options);

  if (ret)
    {
      riscv_override_options_internal (&global_options);
      new_target
	= build_target_option_node (&global_options, &global_options_set);
    }
  else
    new_target = NULL;

  if (fndecl && ret)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;
    }

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);
  return ret;
}
