// { dg-do run }
// { dg-require-profiling "-pg" }
// { dg-options "-pg" }
// { dg-options "-pg -static" { target hppa*-*-hpux* } }
// GROUPS passed profiling
#include <stdio.h>
main()
{
  printf ("PASS\n");
}

/* { dg-final { cleanup-profile-file } } */
