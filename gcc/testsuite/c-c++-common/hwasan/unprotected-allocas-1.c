/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-additional-options "--param hwasan-instrument-allocas=0 -save-temps" } */
/* Only test there's no tagging done when not at -O0.  Without optimisation
   the compiler creates a bunch of other variables on the stack other than the
   vararray/alloca object.
   We also avoid checking when using -flto, since with LTO the compiler can
   recognise the vararray is only used with one size and that size is known at
   compile time -- when the compiler recognises that it instead creates a
   static array, which gets tagged as is expected but not as the test expects.
   */
/* { dg-skip-if "" { *-*-* } { "-O0" "-flto" } { "" } } */

#include "unprotected-allocas-0.c"

/* { dg-final { scan-assembler-not "__hwasan_tag_memory" } } */
