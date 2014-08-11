/* Wrong method to get number of arg reg will cause argument corruption.  */
/* { dg-do run } */
/* { dg-skip-if "incompatible options" { ! { arm_thumb1_ok || arm_thumb2_ok } } { "*" } { "" } } */
/* { dg-require-effective-target arm_eabi } */
/* { dg-options "-mthumb -O1" } */

extern void abort (void);

int foo (int, int, int, int) __attribute__((noinline));

int
foo (int a, int b, int c, int d)
{
  register int m asm ("r8");

  m = a;
  m += b;
  m += c;
  m += d;

  asm ("" : "=r" (m) : "0" (m));

  return m;
}

int
main ()
{
  volatile int a = 10;
  volatile int b = 20;
  volatile int c = 30;
  volatile int d = 40;
  volatile int sum = 0;

  sum = foo (a, b, c, d);

  if (sum != 100)
    abort ();

  return 0;
}
