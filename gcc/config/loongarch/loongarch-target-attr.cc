/* Subroutines used for LoongArch code generation.
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.
   Based on AArch64 target for GNU compiler.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tm_p.h"
#include "diagnostic.h"
#include "opts.h"

/* Enum describing the various ways we can handle attributes.
   In many cases we can reuse the generic option handling machinery.  */

enum loongarch_attr_opt_type
{
  loongarch_attr_mask,	/* Attribute should set a bit in target_flags.  */
  loongarch_attr_enum,	/* Attribute sets an enum variable.  */
  loongarch_attr_bool	/* Attribute sets or unsets a boolean variable.  */
};

/* All the information needed to handle a target attribute.
   NAME is the name of the attribute.
   ATTR_TYPE specifies the type of behavior of the attribute as described
   in the definition of enum loongarch_attr_opt_type.
   ALLOW_NEG is true if the attribute supports a "no-" form.
   OPT_NUM is the enum specifying the option that the attribute modifies.
   This is needed for attributes that mirror the behavior of a command-line
   option, that is it has ATTR_TYPE loongarch_attr_mask.  */

struct loongarch_attribute_info
{
  const char *name;
  unsigned int opt_mask;
  enum loongarch_attr_opt_type attr_type;
  enum opt_code opt_num;
  bool allow_neg;
};

/* Construct a loongarch_attributes from the given arguments.

   OPTS is the name of the compilation option after the "-m" string.

   OPTNUM is the opt_code corresponding to the compilation option.

   OPTMASK is the mask corresponding to the mutation option.  If the
   compilation option does not have a corresponding mask, pass 0.
 */
#define LARCH_ATTR_MASK(OPTS, OPTNUM, OPTMASK)	      \
  {						      \
    OPTS, OPTMASK, loongarch_attr_mask, OPTNUM, true  \
  },

#define LARCH_ATTR_ENUM(OPTS, OPTNUM, OPTMASK)	      \
  {						      \
    OPTS, OPTMASK, loongarch_attr_enum, OPTNUM, false \
  },

#define LARCH_ATTR_BOOL(OPTS, OPTNUM, OPTMASK)	      \
  {						      \
    OPTS, OPTMASK, loongarch_attr_bool, OPTNUM, true  \
  },

/* The target attributes that we support.  */

static const struct loongarch_attribute_info loongarch_attributes[] =
{
  LARCH_ATTR_MASK ("strict-align", OPT_mstrict_align, MASK_STRICT_ALIGN)
  LARCH_ATTR_ENUM ("cmodel", OPT_mcmodel_, 0)
  LARCH_ATTR_ENUM ("arch", OPT_march_, 0)
  LARCH_ATTR_ENUM ("tune", OPT_mtune_, 0)
  LARCH_ATTR_BOOL ("lsx", OPT_mlsx, 0)
  LARCH_ATTR_BOOL ("lasx", OPT_mlasx, 0)
#include "loongarch-evol-attr.def"
  { NULL, 0, loongarch_attr_bool, OPT____, false }
};
#undef LARCH_ATTR_MASK
#undef LARCH_ATTR_ENUM
#undef LARCH_ATTR_BOOL

static void
loongarch_handle_option (struct gcc_options *opts,
			 struct gcc_options *opts_set ATTRIBUTE_UNUSED,
			 const struct cl_decoded_option *decoded,
			 location_t loc ATTRIBUTE_UNUSED,
			 unsigned int opt_mask ATTRIBUTE_UNUSED)
{
  size_t code = decoded->opt_index;
  int val = decoded->value;

  switch (code)
    {
    case OPT_mstrict_align:
      if (val)
	opts->x_target_flags |= opt_mask;
      else
	opts->x_target_flags &= ~opt_mask;
      break;

    case OPT_mcmodel_:
      opts->x_la_opt_cmodel = val;
      break;

    case OPT_march_:
      opts->x_la_opt_cpu_arch = val;

      /* Set these variables to the initial values so that they can be reset
	 in the loongarch_config_target function according to the ARCH
	 settings.  */
      opts->x_la_opt_simd = M_OPT_UNSET;
      opts->x_la_opt_fpu = M_OPT_UNSET;
      opts->x_la_isa_evolution = 0;
      break;

    case OPT_mtune_:
      opts->x_la_opt_cpu_tune = val;

      /* Set these variables to the initial values so that they can be reset
	 in the loongarch_target_option_override function according to the TUNE
	 settings.  */
      opts->x_str_align_functions = NULL;
      opts->x_str_align_loops = NULL;
      opts->x_str_align_jumps = NULL;
      break;

    default:
      gcc_unreachable ();
    }
}

/* Parse ARG_STR which contains the definition of one target attribute.
   Show appropriate errors if any or return true if the attribute is valid.  */

static bool
loongarch_process_one_target_attr (char *arg_str, location_t loc)
{
  bool invert = false;

  size_t len = strlen (arg_str);

  if (len == 0)
    {
      error_at (loc, "malformed %<target()%> pragma or attribute");
      return false;
    }

  char *str_to_check = (char *) alloca (len + 1);
  strcpy (str_to_check, arg_str);

  if (len > 3 && startswith (str_to_check, "no-"))
    {
      invert = true;
      str_to_check += 3;
    }
  char *arg = strchr (str_to_check, '=');

  /* If we found opt=foo then terminate STR_TO_CHECK at the '='
     and point ARG to "foo".  */
  if (arg)
    {
      *arg = '\0';
      arg++;
    }
  const struct loongarch_attribute_info *p_attr;
  bool found = false;
  for (p_attr = loongarch_attributes; p_attr->name; p_attr++)
    {
      /* If the names don't match up, or the user has given an argument
	 to an attribute that doesn't accept one, or didn't give an argument
	 to an attribute that expects one, fail to match.  */
      if (strcmp (str_to_check, p_attr->name) != 0)
	continue;

      found = true;

      /* If the name matches but the attribute does not allow "no-" versions
	 then we can't match.  */
      if (invert && !p_attr->allow_neg)
	{
	  error_at (loc, "pragma or attribute %<target(\"%s\")%> does not "
		    "allow a negated form", str_to_check);
	  return false;
	}

      switch (p_attr->attr_type)
	{
	  /* Either set or unset a boolean option.  */
	case loongarch_attr_mask:
	    {
	      struct cl_decoded_option decoded;

	      /* We only need to specify the option number.
		 loongarch_handle_option will know which mask to apply.  */
	      decoded.opt_index = p_attr->opt_num;
	      decoded.value = !invert;

	      loongarch_handle_option (&global_options, &global_options_set,
				       &decoded, input_location,
				       p_attr->opt_mask);
	      break;
	    }

	  /* Use the option setting machinery to set an option to an enum.  */
	  case loongarch_attr_enum:
	    {
	      if (!arg)
		{
		  error_at (loc, "the value of pragma or attribute "
			    "%<target(\"%s\")%> not be empty", str_to_check);
		  return false;
		}

	      bool valid;
	      int value;
	      struct cl_decoded_option decoded;
	      valid = opt_enum_arg_to_value (p_attr->opt_num, arg,
					      &value, CL_TARGET);

	      decoded.opt_index = p_attr->opt_num;
	      decoded.value = value;

	      if (valid)
		loongarch_handle_option (&global_options,
					 &global_options_set,
					 &decoded, input_location,
					 p_attr->opt_mask);
	      else
		error_at (loc, "pragma or attribute %<target(\"%s=%s\")%> is "
			  "not valid", str_to_check, arg);
	      break;
	    }

	  /* Either set or unset a boolean option.  */
	  case loongarch_attr_bool:
	    {
	      struct cl_decoded_option decoded;

	      generate_option (p_attr->opt_num, NULL, !invert,
			       CL_TARGET, &decoded);
	      switch (decoded.opt_index)
		{
		case OPT_mlsx:
		  global_options.x_la_opt_simd
		    = decoded.value
		    ? (la_opt_simd == ISA_EXT_SIMD_LASX
		       ? ISA_EXT_SIMD_LASX : ISA_EXT_SIMD_LSX)
		    : ISA_EXT_NONE;
		  break;

		case OPT_mlasx:
		  global_options.x_la_opt_simd
		    = decoded.value
		    ? ISA_EXT_SIMD_LASX
		    : (la_opt_simd == ISA_EXT_SIMD_LASX
		       || la_opt_simd == ISA_EXT_SIMD_LSX
		       ? ISA_EXT_SIMD_LSX : ISA_EXT_NONE);
		  break;

		default:
		    {
		      if (decoded.value)
			global_options.x_la_isa_evolution |= p_attr->opt_mask;
		      else
			global_options.x_la_isa_evolution &= ~p_attr->opt_mask;
		      global_options_set.x_la_isa_evolution |= p_attr->opt_mask;
		    }
		}
	      break;
	    }
	default:
	  gcc_unreachable ();
	}
    }

  /* If we reached here we either have found an attribute and validated
     it or didn't match any.  If we matched an attribute but its arguments
     were malformed we will have returned false already.  */
  if (!found)
    error_at (loc, "attribute %<target%> argument %qs is unknown",
	      arg_str);

  return found;
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

bool
loongarch_process_target_attr (tree args, tree fndecl)
{
  location_t loc
    = fndecl == NULL ? UNKNOWN_LOCATION : DECL_SOURCE_LOCATION (fndecl);

  if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree head = TREE_VALUE (args);
	  if (head)
	    {
	      if (!loongarch_process_target_attr (head, fndecl))
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
  auto_vec<char, 32> buffer;
  buffer.safe_grow (len + 1);
  char *str_to_check = buffer.address ();
  memcpy (str_to_check, TREE_STRING_POINTER (args), len + 1);

  if (len == 0)
    {
      error_at (loc, "malformed %<target()%> pragma or attribute");
      return false;
    }

  /* Used to catch empty spaces between commas i.e.
     attribute ((target ("attr1,,attr2"))).  */
  unsigned int num_commas = num_occurences_in_str (',', str_to_check);

  /* Handle multiple target attributes separated by ','.  */
  char *token = strtok_r (str_to_check, ",", &str_to_check);

  unsigned int num_attrs = 0;
  while (token)
    {
      num_attrs++;
      if (!loongarch_process_one_target_attr (token, loc))
	return false;

      token = strtok_r (NULL, ",", &str_to_check);
    }

  if (num_attrs != num_commas + 1)
    {
      error_at (loc, "malformed %<target(\"%s\")%> pragma or attribute",
		TREE_STRING_POINTER (args));
      return false;
    }

  return true;
}

/* Implement TARGET_OPTION_VALID_ATTRIBUTE_P.  This is used to
   process attribute ((target ("..."))).  */

bool
loongarch_option_valid_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree old_optimize;
  tree new_target, new_optimize;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* If what we're processing is the current pragma string then the
     target option node is already stored in target_option_current_node
     by loongarch_pragma_target_parse in loongarch-target-attr.cc.
     Use that to avoid having to re-parse the string.  */
  if (!existing_target && args == current_target_pragma)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = target_option_current_node;
      return true;
    }

  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  old_optimize
    = build_optimization_node (&global_options, &global_options_set);

  /* If the function changed the optimization levels as well as setting
     target options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (func_optimize));

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

  ret = loongarch_process_target_attr (args, fndecl);

  /* Set up any additional state.  */
  if (ret)
    {
      loongarch_option_override_internal (&la_target,
					  &global_options,
					  &global_options_set);
      new_target = build_target_option_node (&global_options,
					     &global_options_set);
    }
  else
    new_target = NULL;

  new_optimize = build_optimization_node (&global_options,
					  &global_options_set);

  if (fndecl && ret)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);

  if (old_optimize != new_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (old_optimize));
  return ret;
}

