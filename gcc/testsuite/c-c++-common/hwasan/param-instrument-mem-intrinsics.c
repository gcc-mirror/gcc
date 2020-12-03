/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } { "" } } */
/* { dg-additional-options "--param hwasan-instrument-mem-intrinsics=0" } */

#include "builtin-special-handling.c"

/* With this flag there should be no checking of builtins.
   The above file only has builtins, and hence there should be no checking
   after compilation.  */
/* { dg-final { scan-assembler-not "__hwasan_(load|store)" } } */
