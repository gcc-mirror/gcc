/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-mstackrealign -O2" } */

extern void abort (void);

__attribute__((noinline)) static void foo (int i1, int i2, int i3)
{
  if (i3 != 3)
    abort ();
}

int main (int argc, char **argv)
{
  foo (1, 2, 3);
  return 0;
}
