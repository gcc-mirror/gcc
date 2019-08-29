/* { dg-do run } */
/* { dg-additional-options "-Wno-volatile" { target c++ } } */

/* Even on targets that don't need the optional failval parameter,
   side-effects on the operand should still be calculated.  */

int x = 3;
volatile int y = 9;

int main ()
{
  int z = __builtin_speculation_safe_value (x, y++);
  if (z != 3 || y != 10)
    __builtin_abort ();
  return 0;
}

/* { dg-prune-output "this target does not define a speculation barrier;" } */
