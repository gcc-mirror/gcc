/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

#include <altivec.h>

extern void abort (void);

vector signed short
doString(vector signed short *vp)
{
  /* Iteration replaces tail recursion with -O2.  */
  vector signed short result = vec_strir (*vp);
  if (vec_strir_p (*vp))
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
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0x0, 0xd, 0xe }
  };

  vector signed short expected0 =
    { 0x1, 0x3, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short expected1 =
    { 0x0, 0x0, 0x5, 0x7, 0x9, 0xb, 0xd, 0xf };
  vector signed short expected2 =
    { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };
  vector signed short expected3 =
    { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xd, 0xe };

  if (!vec_all_eq (doString (&composed_string[0]), expected1))
    abort ();
  if (!vec_all_eq (doString (&composed_string[1]), expected1))
    abort ();
  if (!vec_all_eq (doString (&composed_string[2]), expected2))
    abort ();
  if (!vec_all_eq (doString (&composed_string[3]), expected3))
    abort ();
}
