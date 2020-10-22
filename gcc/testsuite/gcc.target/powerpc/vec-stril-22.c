/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O1 -mdejagnu-cpu=power10" } */
/* See vec-stril-23.c for the same test with -O2 optimization.  */

#include <altivec.h>

extern void abort (void);

vector signed short
doString(vector signed short *vp)
{
  /* Though two built-in functions are called, the implementation
     should use a single instruction to implement both with -O1.  */
  vector signed short result = vec_stril (*vp);
  if (vec_stril_p (*vp))
    return result;
  else
    return doString (vp + 1);
}

int main (int argc, short *argv [])
{
  vector signed short composed_string [4] = {
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf },
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf },
    { 0x1, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 },
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 }
  };

  vector signed short expected0 = { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short expected1 = { 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
  vector signed short expected2 = { 0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
  vector signed short expected3 = { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0x0 };

  if (!vec_all_eq (doString (&composed_string[0]), expected1))
    abort ();
  if (!vec_all_eq (doString (&composed_string[1]), expected1))
    abort ();
  if (!vec_all_eq (doString (&composed_string[2]), expected2))
    abort ();
  if (!vec_all_eq (doString (&composed_string[3]), expected3))
    abort ();
}
