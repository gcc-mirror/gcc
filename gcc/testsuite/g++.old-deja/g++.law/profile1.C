// { dg-do run { xfail i[3456]86-*-sco3.2v5* } }
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
