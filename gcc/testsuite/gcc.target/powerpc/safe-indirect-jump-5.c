/* { dg-do run } */
/* { dg-additional-options "-mno-speculate-indirect-jumps -Wno-pedantic" } */
/* { dg-warning "'-mno-speculate-indirect-jumps' is deprecated" "" { target *-*-* } 0 } */

/* Test for deliberate misprediction of computed goto.  */

int __attribute__((noinline)) bar (int i)
{
  return 1960 + i;
}

int __attribute__((noinline)) baz (int i)
{
  return i * i;
}

int __attribute__((noinline)) spaz (int i)
{
  return i + 1;
}

int foo (int x)
{
  static void *labptr[] = { &&lab0, &&lab1, &&lab2 };

  if (x < 0 || x > 2)
    return -1;

  goto *labptr[x];

 lab0:
  return bar (x);

 lab1:
  return baz (x) + 1;

 lab2:
  return spaz (x) / 2;
}

int main ()
{
  if (foo (0) != 1960)
    __builtin_abort ();

  if (foo (1) != 2)
    __builtin_abort ();

  if (foo (2) != 1)
    __builtin_abort ();

  if (foo (3) != -1)
    __builtin_abort ();

  return 0;
}
