/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-march=i386 -O3 -fomit-frame-pointer" } */

/* For this test case, we used to do an invalid load motion after
   reload, because we missed autoincrements of the stack pointer.  */

extern void abort (void);

static int j;

static void __attribute__((noinline))
f1 (int a, int b, int c, int d, int e)
{
  j = a;
}

int __attribute__((noinline))
f2 (int a, int b, int c, int d, int e)
{
  if ((b & 0x1111) != 1)
    f1 (a, b, c, d, e);
  return 0;
}

int
main (void)
{
  f2 (123, 0, 0, 0, 0);
  if (j != 123)
    abort ();
  return 0;
}

