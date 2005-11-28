// { dg-do run }
// { dg-require-profiling "-pg" }
// { dg-options "-pg" }
// { dg-options "-pg -static" { target hppa*-*-hpux* } }
// { dg-bogus "\[Uu\]nresolved symbol ._mcount" "Profiling unsupported" { xfail *-*-netware* } 0 }
// GROUPS passed profiling
#include <stdio.h>
main()
{
  printf ("PASS\n");
}
