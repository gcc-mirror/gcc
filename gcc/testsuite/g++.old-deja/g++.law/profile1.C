// { dg-do run { xfail mips*-*-* i[3456]86-*-sco3.2v5* } }
// { dg-require-profiling "" }
// { dg-options "-pg" }
// { dg-options "-pg -static" { target hppa*-*-hpux* } }
// GROUPS passed profiling
#include <stdio.h>
main()
{
  printf ("PASS\n");
}
