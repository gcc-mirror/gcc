/* Copyright (C) 2010  Free Software Foundation.

   Check the runtime behavior of the C library's cproj() function and
   whether it follows the standard.  Versions of GLIBC through 2.11.1
   had an incorrect implementation which will conflict with GCC's
   builtin cproj().  GLIBC 2.12+ should be okay.

   Origin: Kaveh R. Ghazi,  April 20, 2010.  */

/* { dg-do run } */
/* { dg-options "-fno-builtin-cproj" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target c99_runtime } */

extern void abort(void);
extern void exit(int);
double _Complex cproj(double _Complex);

int main (void)
{
  if (cproj (2+3i) != 2+3i)
    abort();

  exit(0);
}
