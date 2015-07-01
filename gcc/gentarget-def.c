/* Generate insn-target-def.h, an automatically-generated part of targetm.
   Copyright (C) 1987-2015 Free Software Foundation, Inc.

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
  for (const char *p = prototype; *p; ++p)
    if (*p == 'x' && ISDIGIT (p[1]))
      {
	/* This should be a parameter name of the form "x<OPNO>".
	   That doesn't contribute to the suffix, so skip ahead and
	   process the following character.  */
	char *endptr;
	if ((unsigned int) strtol (p + 1, &endptr, 10) != opno
	    || (*endptr != ',' && *endptr != ')'))
	  {
	    error ("invalid prototype for '%s'", name);
	    exit (FATAL_EXIT_CODE);
	  }
	opno += 1;
	p = endptr;
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
	      print_c_condition (test);
	      printf (";\n");
	      printf ("}\n");
	    }
	  have_name = entry;
	}
      printf ("\nstatic rtx_insn *\n");
      printf ("target_gen_%s %s\n", name, prototype);
      printf ("{\n");
      if (truth < 0)
	printf ("  gcc_checking_assert (targetm.have_%s ());\n", name);
      printf ("  return insnify (gen_%s (", name);
      for (i = 0; i < opno; ++i)
	printf ("%sx%d", i == 0 ? "" : ", ", i);
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
	  const char *p = prototype;
	  while (*p)
	    {
	      if (p[0] == 'x' && ISDIGIT (p[1]))
		{
		  char *endptr;
		  strtol (p + 1, &endptr, 10);
		  p = endptr;
		}
	      else
		fputc (*p++, stdout);
	    }
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
}

int
main (int argc, char **argv)
{
  int insn_code_number = 0;

  progname = "gentarget-def";

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  insns = new hash_table <insn_hasher> (31);
  stubs = new hash_table <nofree_string_hash> (31);
  have_funcs = new hash_map <nofree_string_hash, const char *>;

  while (1)
    {
      int line_no;
      rtx desc = read_md_rtx (&line_no, &insn_code_number);
      if (desc == NULL)
	break;
      if (GET_CODE (desc) == DEFINE_INSN || GET_CODE (desc) == DEFINE_EXPAND)
	{
	  const char *name = XSTR (desc, 0);
	  if (name[0] != 0 && name[0] != '*')
	    {
	      hashval_t hash = htab_hash_string (name);
	      rtx *slot = insns->find_slot_with_hash (name, hash, INSERT);
	      if (*slot)
		{
		  message_with_line (line_no, "duplicate definition of '%s'",
				     name);
		  have_error = 1;
		}
	      else
		*slot = desc;
	    }
	}
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
