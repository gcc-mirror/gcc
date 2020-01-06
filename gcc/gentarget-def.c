/* Generate insn-target-def.h, an automatically-generated part of targetm.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"
#include "hash-table.h"

/* This class hashes define_insns and define_expands by name.  */
struct insn_hasher : nofree_ptr_hash <rtx_def>
{
  typedef rtx value_type;
  typedef const char *compare_type;

  static inline hashval_t hash (rtx);
  static inline bool equal (rtx, const char *);
};

hashval_t
insn_hasher::hash (rtx x)
{
  return htab_hash_string (XSTR (x, 0));
}

bool
insn_hasher::equal (rtx x, const char *y)
{
  return strcmp (XSTR (x, 0), y) == 0;
}

/* All define_insns and define_expands, hashed by name.  */
static hash_table <insn_hasher> *insns;

/* Records the prototype suffix X for each invalid_X stub that has been
   generated.  */
static hash_table <nofree_string_hash> *stubs;

/* Records which C conditions have been wrapped in functions, as a mapping
   from the C condition to the function name.  */
static hash_map <nofree_string_hash, const char *> *have_funcs;

/* Return true if the part of the prototype at P is for an argument
   name.  If so, point *END_OUT to the first character after the name.
   If OPNO_OUT is nonnull, set *OPNO_OUT to the number of the associated
   operand.  If REQUIRED_OUT is nonnull, set *REQUIRED_OUT to whether the
   .md pattern is required to match the operand.  */

static bool
parse_argument (const char *p, const char **end_out,
		unsigned int *opno_out = 0,
		bool *required_out = 0)
{
  while (ISSPACE (*p))
    p++;
  if (p[0] == 'x' && ISDIGIT (p[1]))
    {
      p += 1;
      if (required_out)
	*required_out = true;
    }
  else if (p[0] == 'o' && p[1] == 'p' && p[2] == 't' && ISDIGIT (p[3]))
    {
      p += 3;
      if (required_out)
	*required_out = false;
    }
  else
    return false;

  char *endptr;
  unsigned int opno = strtol (p, &endptr, 10);
  if (opno_out)
    *opno_out = opno;
  *end_out = endptr;
  return true;
}


/* Output hook definitions for pattern NAME, which has target-insns.def
   prototype PROTOTYPE.  */

static void
def_target_insn (const char *name, const char *prototype)
{
  /* Get an upper-case form of NAME.  */
  unsigned int i;
  char *upper_name = XALLOCAVEC (char, strlen (name) + 1);
  for (i = 0; name[i]; ++i)
    upper_name[i] = TOUPPER (name[i]);
  upper_name[i] = 0;

  /* Check that the prototype is valid and concatenate the types
     together to get a suffix.  */
  char *suffix = XALLOCAVEC (char, strlen (prototype) + 1);
  i = 0;
  unsigned int opno = 0;
  unsigned int required_ops = 0;
  unsigned int this_opno;
  bool required_p;
  for (const char *p = prototype; *p; ++p)
    if (parse_argument (p, &p, &this_opno, &required_p))
      {
	if (this_opno != opno || (*p != ',' && *p != ')'))
	  {
	    error ("invalid prototype for '%s'", name);
	    exit (FATAL_EXIT_CODE);
	  }
	if (required_p && required_ops < opno)
	  {
	    error ("prototype for '%s' has required operands after"
		   " optional operands", name);
	    exit (FATAL_EXIT_CODE);
	  }
	opno += 1;
	if (required_p)
	  required_ops = opno;
	/* Skip over ')'s.  */
	if (*p == ',')
	  suffix[i++] = '_';
      }
    else if (*p == ')' || *p == ',')
      {
	/* We found the end of a parameter without finding a
	   parameter name.  */
	if (strcmp (prototype, "(void)") != 0)
	  {
	    error ("argument %d of '%s' did not have the expected name",
		   opno, name);
	    exit (FATAL_EXIT_CODE);
	  }
      }
    else if (*p != '(' && !ISSPACE (*p))
      suffix[i++] = *p;
  suffix[i] = 0;

  /* See whether we have an implementation of this pattern.  */
  hashval_t hash = htab_hash_string (name);
  int truth = 0;
  const char *have_name = name;
  if (rtx insn = insns->find_with_hash (name, hash))
    {
      pattern_stats stats;
      get_pattern_stats (&stats, XVEC (insn, 1));
      unsigned int actual_ops = stats.num_generator_args;
      if (opno == required_ops && opno != actual_ops)
	error_at (get_file_location (insn),
		  "'%s' must have %d operands (excluding match_dups)",
		  name, required_ops);
      else if (actual_ops < required_ops)
	error_at (get_file_location (insn),
		  "'%s' must have at least %d operands (excluding match_dups)",
		  name, required_ops);
      else if (actual_ops > opno)
	error_at (get_file_location (insn),
		  "'%s' must have no more than %d operands"
		  " (excluding match_dups)", name, opno);

      const char *test = XSTR (insn, 2);
      truth = maybe_eval_c_test (test);
      gcc_assert (truth != 0);
      if (truth < 0)
	{
	  /* Try to reuse an existing function that performs the same test.  */
	  bool existed;
	  const char *&entry = have_funcs->get_or_insert (test, &existed);
	  if (!existed)
	    {
	      entry = name;
	      printf ("\nstatic bool\n");
	      printf ("target_have_%s (void)\n", name);
	      printf ("{\n");
	      printf ("  return ");
	      rtx_reader_ptr->print_c_condition (test);
	      printf (";\n");
	      printf ("}\n");
	    }
	  have_name = entry;
	}
      printf ("\nstatic rtx_insn *\n");
      printf ("target_gen_%s ", name);
      /* Print the prototype with the argument names after ACTUAL_OPS
	 removed.  */
      const char *p = prototype, *end;
      while (*p)
	if (parse_argument (p, &end, &this_opno) && this_opno >= actual_ops)
	  p = end;
	else
	  fputc (*p++, stdout);

      printf ("\n{\n");
      if (truth < 0)
	printf ("  gcc_checking_assert (targetm.have_%s ());\n", name);
      printf ("  return insnify (gen_%s (", name);
      for (i = 0; i < actual_ops; ++i)
	printf ("%s%s%d", i == 0 ? "" : ", ",
		i < required_ops ? "x" : "opt", i);
      printf ("));\n");
      printf ("}\n");
    }
  else
    {
      const char **slot = stubs->find_slot (suffix, INSERT);
      if (!*slot)
	{
	  *slot = xstrdup (suffix);
	  printf ("\nstatic rtx_insn *\n");
	  printf ("invalid_%s ", suffix);
	  /* Print the prototype with the argument names removed.  */
	  const char *p = prototype;
	  while (*p)
	    if (!parse_argument (p, &p))
	      fputc (*p++, stdout);
	  printf ("\n{\n");
	  printf ("  gcc_unreachable ();\n");
	  printf ("}\n");
	}
    }
  printf ("\n#undef TARGET_HAVE_%s\n", upper_name);
  printf ("#define TARGET_HAVE_%s ", upper_name);
  if (truth == 0)
    printf ("hook_bool_void_false\n");
  else if (truth == 1)
    printf ("hook_bool_void_true\n");
  else
    printf ("target_have_%s\n", have_name);

  printf ("#undef TARGET_GEN_%s\n", upper_name);
  printf ("#define TARGET_GEN_%s ", upper_name);
  if (truth == 0)
    printf ("invalid_%s\n", suffix);
  else
    printf ("target_gen_%s\n", name);

  printf ("#undef TARGET_CODE_FOR_%s\n", upper_name);
  printf ("#define TARGET_CODE_FOR_%s ", upper_name);
  if (truth == 0)
    printf ("CODE_FOR_nothing\n");
  else
    printf ("CODE_FOR_%s\n", name);
}

/* Record the DEFINE_INSN or DEFINE_EXPAND described by INFO.  */

static void
add_insn (md_rtx_info *info)
{
  rtx def = info->def;
  const char *name = XSTR (def, 0);
  if (name[0] == 0 || name[0] == '*')
    return;

  hashval_t hash = htab_hash_string (name);
  rtx *slot = insns->find_slot_with_hash (name, hash, INSERT);
  if (*slot)
    error_at (info->loc, "duplicate definition of '%s'", name);
  else
    *slot = def;
}

int
main (int argc, const char **argv)
{
  progname = "gentarget-def";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  insns = new hash_table <insn_hasher> (31);
  stubs = new hash_table <nofree_string_hash> (31);
  have_funcs = new hash_map <nofree_string_hash, const char *>;

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
      case DEFINE_EXPAND:
	add_insn (&info);
	break;

      default:
	break;
      }

  printf ("/* Generated automatically by the program `gentarget-def'.  */\n");
  printf ("#ifndef GCC_INSN_TARGET_DEF_H\n");
  printf ("#define GCC_INSN_TARGET_DEF_H\n");

  /* Output a routine to convert an rtx to an rtx_insn sequence.
     ??? At some point the gen_* functions themselves should return
	 rtx_insns.  */
  printf ("\nstatic inline rtx_insn *\n");
  printf ("insnify (rtx x)\n");
  printf ("{\n");
  printf ("  if (!x)\n");
  printf ("    return NULL;\n");
  printf ("  if (rtx_insn *insn = dyn_cast <rtx_insn *> (x))\n");
  printf ("    return insn;\n");
  printf ("  start_sequence ();\n");
  printf ("  emit (x, false);\n");
  printf ("  rtx_insn *res = get_insns ();\n");
  printf ("  end_sequence ();\n");
  printf ("  return res;\n");
  printf ("}\n");

#define DEF_TARGET_INSN(INSN, ARGS) \
  def_target_insn (#INSN, #ARGS);
#include "target-insns.def"
#undef DEF_TARGET_INSN

  printf ("\n#endif /* GCC_INSN_TARGET_DEF_H */\n");

  if (have_error || ferror (stdout) || fflush (stdout) || fclose (stdout))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}
