/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mno-vsx" } */

#include <altivec.h>

/* This program's "test for excess errors" demonstrates that combining
   the target options -mcpu=power9 and -mno-vsx does not
   result in an error.  A previous version of the compiler aborted
   with the error message:

      "power9-dform requires power9-vector."

   when these two options were used in combination.

   The newer version of the compiler, instead, automatically disables
   power9-dform when the -mno-vsx command-line option is
   specified.  */
int
test_any_equal (vector bool char *arg1_p, vector bool char *arg2_p)
{
  vector bool char arg_1 = *arg1_p;
  vector bool char arg_2 = *arg2_p;

  return vec_any_eq (arg_1, arg_2);
}

