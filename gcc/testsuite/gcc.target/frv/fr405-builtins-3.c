/* Test the remaining integer instructions.  */
/* { dg-options "-mcpu=fr405" } */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

int main ()
{
  if (__SLASS (0x112233, 4) != 0x1122330)
    abort ();

  if (__SLASS (0x7ffff, 12) != 0x7ffff000)
    abort ();

  if (__SLASS (0x80000, 12) != 0x7fffffff)
    abort ();

  if (__SLASS (-0x7ffff, 12) != -0x7ffff000)
    abort ();

  if (__SLASS (-0x80000, 12) != -0x7fffffff - 1)
    abort ();

  if (__SLASS (-0x80001, 12) != -0x7fffffff - 1)
    abort ();

  if (__ADDSS (0x7fffffff, 1) != 0x7fffffff)
    abort ();

  if (__ADDSS (0x7ffffffd, 1) != 0x7ffffffe)
    abort ();

  if (__ADDSS (-0x7fffffff, -2) != -0x7fffffff - 1)
    abort ();

  if (__ADDSS (-0x7ffffffd, -2) != -0x7fffffff)
    abort ();

  if (__SUBSS (0x7fffffff, -1) != 0x7fffffff)
    abort ();

  if (__SUBSS (0x7ffffffd, -1) != 0x7ffffffe)
    abort ();

  if (__SUBSS (-0x7fffffff, 2) != -0x7fffffff - 1)
    abort ();

  if (__SUBSS (-0x7ffffffd, 2) != -0x7fffffff)
    abort ();

  if (__SCAN (0x12345678, 0) != 3)
    abort ();

  if (__SCAN (0x12345678, 0x24680000) != 17)
    abort ();

  exit (0);
}
