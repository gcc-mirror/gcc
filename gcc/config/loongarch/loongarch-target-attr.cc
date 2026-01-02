/* Subroutines used for LoongArch code generation.
   Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

#define INCLUDE_STRING
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

/* Describes the priority of each feature.  The larger the value, the higher
   the priority.  The priority setting rule is vector priority.

   The highest priority currently is "-mlasx".
   The second highest is "-march=la64v1.1" (lsx and la64v1.1 enabled
   instructions).
   The third highest is "-mlsx".
 */
enum features_prio
{
  LA_PRIO_NONE = 0,
  LA_PRIO_LOONGARCH64,
  LA_PRIO_STRICT_ALIGN,
  LA_PRIO_FRECIPE,
  LA_PRIO_DIV32 = LA_PRIO_FRECIPE,
  LA_PRIO_LAM_BH = LA_PRIO_FRECIPE,
  LA_PRIO_LAMCAS = LA_PRIO_FRECIPE,
  LA_PRIO_SCQ = LA_PRIO_FRECIPE,
  LA_PRIO_LD_SEQ_SA = LA_PRIO_FRECIPE,
  LA_PRIO_LSX,
  LA_PRIO_LA64V1_0,
  LA_PRIO_LA64V1_1,
  LA_PRIO_LASX,
  LA_PRIO_MAX
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
  const loongarch_fmv_feature_mask feat_mask;
  const unsigned int arch_ver;
  enum features_prio priority;
};

/* Construct a loongarch_attributes from the given arguments.

   OPTS is the name of the compilation option after the "-m" string.

   OPTNUM is the opt_code corresponding to the compilation option.

   OPTMASK is the mask corresponding to the mutation option.  If the
   compilation option does not have a corresponding mask, pass 0.
 */
#define LARCH_ATTR_MASK(OPTS, OPTNUM, OPTMASK, FEATMASK, PRIO)		      \
{									      \
  OPTS, OPTMASK, loongarch_attr_mask, OPTNUM, true, 1ULL << FEATMASK,	      \
  N_ARCH_TYPES,	PRIO							      \
},

#define LARCH_ATTR_ENUM(OPTS, OPTNUM, PRIO)				      \
{									      \
  OPTS, 0, loongarch_attr_enum, OPTNUM, false, 0, N_ARCH_TYPES, PRIO	      \
},

#define LARCH_ATTR_BOOL(OPTS, OPTNUM, OPTMASK, FEATMASK, ARCH_V, PRIO)	      \
{									      \
  OPTS, OPTMASK, loongarch_attr_bool, OPTNUM, true, 1ULL << FEATMASK, ARCH_V, \
  PRIO									      \
},

/* The target attributes that we support.  */

static const struct loongarch_attribute_info loongarch_attributes[] =
{
  LARCH_ATTR_MASK ("strict-align", OPT_mstrict_align, MASK_STRICT_ALIGN,
		   FEAT_UAL, LA_PRIO_STRICT_ALIGN)
  LARCH_ATTR_ENUM ("cmodel", OPT_mcmodel_, LA_PRIO_NONE)
  LARCH_ATTR_ENUM ("arch", OPT_march_, LA_PRIO_NONE)
  LARCH_ATTR_ENUM ("tune", OPT_mtune_, LA_PRIO_NONE)
  LARCH_ATTR_BOOL ("lsx", OPT_mlsx, 0, FEAT_LSX, ARCH_LA64V1_0, LA_PRIO_LSX)
  LARCH_ATTR_BOOL ("lasx", OPT_mlasx, 0, FEAT_LASX | FEAT_LSX, 0, LA_PRIO_LASX)
#include "loongarch-evol-attr.def"
  { NULL, 0, loongarch_attr_bool, OPT____, false, 0, N_ARCH_TYPES, LA_PRIO_NONE }
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

/* Parse a function multiversioning feature string STR, as found in a
   target_version or target_clones attribute.

   If FEATURE_MASK is nonnull, then assign to it a bitmask representing
   the set of features explicitly specified in the feature string.

   If FEATURE_PRIORITY is nonnull, set one or two unsigned integer values
   presenting the priority of the feature string.  When the priority is
   set explicitly in the attribute string, the number of members of
   feature_priority is 2, feature_priority[0] is the priority set in the
   code, and feature_priority[1] is the priority calculated from the
   feature string.  When the priority is not set in the attribute string,
   the number of members of feature_priority is 1, and its value is the
   priority calculated by the feature string.  */

bool
loongarch_parse_fmv_features (tree decl, string_slice str,
			      loongarch_fmv_feature_mask *feature_mask,
			      auto_vec<unsigned int> *feature_priority)
{
  location_t loc
    = decl == NULL ? UNKNOWN_LOCATION : DECL_SOURCE_LOCATION (decl);

  if (feature_mask)
    *feature_mask = 0;

  string_slice attr_str = string_slice::tokenize (&str, ";");
  attr_str = attr_str.strip ();

  if (attr_str == "default")
    {
      if (str.is_valid ())
	{
	  error_at (loc, "\"default\" cannot be set together with other "
		    "features in %qs", attr_str.begin ());
	  return false;
	}

      if (feature_priority)
	feature_priority->safe_push (LA_PRIO_NONE);
      return true;
    }

  if (attr_str.empty ())
    {
      error_at (loc, "characher before %<;%> in attribute %qs cannot be empty",
		attr_str.begin ());
      return false;
    }

  /* At this time, str stores the string with the priority set in attribute.
     If it does not exist, it is illegal.  */
  if (str.is_valid ())
    {
      if (str.empty ())
	{
	  error_at (loc, "in attribute %qs priority cannot be empty",
		    attr_str.begin ());
	  return false;
	}

      string_slice prio_str = string_slice::tokenize (&str, ";");

      if (str.is_valid ())
	{
	  error_at (loc, "in attribute %qs the number of reatures "
		    "cannot exceed two", attr_str.begin ());
	  return false;
	}

      prio_str = prio_str.strip ();
      string_slice name = string_slice::tokenize (&prio_str, "=");

      if (name == "priority" && prio_str.is_valid ())
	{
	  unsigned int tmp_prio = 0;
	  unsigned int len = 0;

	  for (char c : prio_str)
	    {
	      if (ISDIGIT (c))
		len++;
	      else
		break;
	    }

	  if (len != prio_str.size ()
	      || sscanf (prio_str.begin (), "%u", &tmp_prio) != 1)
	    {
	      error_at (loc, "Setting the priority value to %qs is "
			"illegal in attribute %qs", prio_str.begin (),
			attr_str.begin ());
	      return false;
	    }

	  if (feature_priority)
	    feature_priority->safe_push (tmp_prio + LA_PRIO_MAX);
	}
      else
	{
	  error_at (loc, "in attribute %qs, the second feature should be "
		    "\"priority=%<num%>\" instead of %qs", attr_str.begin (),
		    name.begin ());
	  return false;
	}
    }

  if (attr_str.is_valid ())
    {
      int num_features = ARRAY_SIZE (loongarch_attributes);

      /* Handle arch= if specified.  For priority, set it to be 1 more than
	 the best instruction set the processor can handle.  */
      if (strstr (attr_str.begin (), "arch=") != NULL)
	{
	  string_slice arch_name = attr_str;
	  string_slice::tokenize (&arch_name, "=");
	  if (!arch_name.is_valid ())
	    {
	      error_at (loc, "in attribute %qs you need to set a legal value "
			"for \"arch\"", attr_str.begin ());
	      return false;
	    }

	  loongarch_fmv_feature_mask tmp_mask = 0ULL;
	  unsigned int tmp_prio = 0;

	  if (arch_name == "loongarch64")
	    {
	      tmp_mask = 1UL << FEAT_LA64;
	      tmp_prio = LA_PRIO_LOONGARCH64;
	    }
	  else if (arch_name == "la64v1.0")
	    {
	      tmp_mask = 1ULL << FEAT_LA64;
	      for (int i = 0; i < num_features; i++)
		if (loongarch_attributes[i].arch_ver == ARCH_LA64V1_0)
		  tmp_mask |= loongarch_attributes[i].feat_mask;
	      tmp_prio = LA_PRIO_LA64V1_0;
	    }
	  else if (arch_name == "la64v1.1")
	    {
	      tmp_mask = 1ULL << FEAT_LA64;
	      for (int i = 0; i < num_features; i++)
		if (loongarch_attributes[i].arch_ver == ARCH_LA64V1_0
		    || loongarch_attributes[i].arch_ver == ARCH_LA64V1_1)
		  tmp_mask |= loongarch_attributes[i].feat_mask;
	      tmp_prio = LA_PRIO_LA64V1_1;
	    }
	  else
	    {
	      error_at (loc, "in attribute %qs you need to set a legal value "
			"for \"arch\"", attr_str.begin ());
	      return false;
	    }

	  if (feature_mask)
	    *feature_mask = tmp_mask;

	  if (feature_priority)
	    feature_priority->safe_push (tmp_prio);
	}
      else
	{
	  int i;
	  for (i = 0; i < num_features - 1; i++)
	    {
	      if (loongarch_attributes[i].name == attr_str
		  || strstr (attr_str.begin (),
			     loongarch_attributes[i].name) != NULL)
		{
		  if (loongarch_attributes[i].feat_mask == 0)
		    {
		      error_at (loc, "attribute %qs is not supported in "
				"%<target_version%> or %<target_clones%>",
				attr_str.begin ());
		      return false;
		    }

		  if (feature_mask)
		    *feature_mask = loongarch_attributes[i].feat_mask;

		  if (feature_priority)
		    feature_priority->safe_push (loongarch_attributes[i].priority);
		  break;
		}
	    }

	  if (i == num_features - 1)
	    {
	      error_at (loc, "%qs is not supported in target attribute",
			attr_str.begin ());
	      return false;
	    }
	}
    }

  if (feature_priority)
    gcc_assert (feature_priority->length () == 1
		|| feature_priority->length () == 2);

  return true;
}

/* Compare priorities of two version decls. Return:
    1: decl1 has a higher priority
   -1: decl2 has a higher priority
    0: decl1 and decl2 have the same priority.
*/

int
loongarch_compare_version_priority (tree decl1, tree decl2)
{
  auto_vec<unsigned int> prio1, prio2;

  get_feature_mask_for_version (decl1, NULL, &prio1);
  get_feature_mask_for_version (decl2, NULL, &prio2);

  unsigned int max_prio1
    = prio1.length () == 2 ? MAX (prio1[0], prio1[1]) : prio1[0];
  unsigned int max_prio2
    = prio2.length () == 2 ? MAX (prio2[0], prio2[1]) : prio2[0];

  if (max_prio1 != max_prio2)
    return max_prio1 > max_prio2 ? 1 : -1;

  /* If max_prio1 == max_prio2, and max_prio1 >= LA_PRIO_MAX,
     it means that the attribute strings of decl1 and decl2 are both
     set with priorities, and the priority values are the same.
     So next we use the priority calculated by the attribute string to
     compare.  */
  if (max_prio1 >= LA_PRIO_MAX)
    {
      unsigned int min_prio1
	= prio1.length () == 2 ? MIN (prio1[0], prio1[1]) : prio1[0];
      unsigned int min_prio2
	= prio2.length () == 2 ? MIN (prio2[0], prio2[1]) : prio2[0];

      if (min_prio1 != min_prio2)
	return min_prio1 > min_prio2 ? 1 : -1;
    }

  return 0;
}
