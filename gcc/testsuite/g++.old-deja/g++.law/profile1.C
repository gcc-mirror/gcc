// { dg-do run }
// { dg-require-profiling "-pg" }
// { dg-options "-pg" }
// { dg-options "-pg -static" { target hppa*-*-hpux* } }
// GROUPS passed profiling
#include <stdio.h>

int
main()
{
  printf ("PASS\n");
  return 0;
}

/* { dg-final { cleanup-profile-file } } */
