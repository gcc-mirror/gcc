// { dg-do run { xfail m68k-motorola-sysv m88k-motorola-sysv3 mips*-*-* i[3456]86-*-sco3.2v5* } }
// { dg-options "-pg" }
// { dg-options "-pg -static" { target hppa*-*-hpux* } }
// GROUPS passed profiling
#include <stdio.h>
main()
{
  printf ("PASS\n");
}
