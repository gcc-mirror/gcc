// Special g++ Options: -pg
// GROUPS passed profiling
// Skip if not native
// excess errors test - XFAIL m68k-motorola-sysv m88k-motorola-sysv3 mips*-*-* i[3456]86-*-sco3.2v5*
#include <stdio.h>
main()
{
  printf ("PASS\n");
}
