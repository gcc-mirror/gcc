/* Copyright (C) 2004 Free Software Foundation.

   PR other/15526
   Verify correct overflow checking with -ftrapv.

   Written by Falk Hueffner, 20th May 2004.  */

/* { dg-do run } */
/* { dg-options "-ftrapv" } */
/* { dg-require-effective-target trapping } */

__attribute__((noinline)) int
mulv(int a, int b)
{
  return a * b;
}

int
main()
{
  mulv( 0,  0);
  mulv( 0, -1);
  mulv(-1,  0);
  mulv(-1, -1);
  return 0;
}
