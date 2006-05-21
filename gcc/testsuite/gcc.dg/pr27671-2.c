/* PR rtl-optimization/27671.
   The combiner used to simplify "a ^ b == a" to "a" via
   simplify_relational_operation_1 in simplify-rtx.c.  */
/* { dg-do run } */
/* { dg-options "-O1" } */
/* { dg-options "-O1 -march=pentium4" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

extern void abort (void) __attribute__ ((noreturn));
extern void exit (int) __attribute__ ((noreturn));

static void __attribute__ ((noinline))
bar (int k)
{
  int n = k % 2;
  if (n == 0)
    abort ();
}

int
main (void)
{  
  bar (1);
  exit (0);
}
