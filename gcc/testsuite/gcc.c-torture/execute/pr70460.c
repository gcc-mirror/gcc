/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */
/* { dg-skip-if "label differences not supported" { avr-*-* } } */

/* PR rtl-optimization/70460 */

int c;

__attribute__((noinline, noclone)) void
foo (int x)
{
  static int b[] = { &&lab1 - &&lab0, &&lab2 - &&lab0 };
  void *a = &&lab0 + b[x];
  goto *a;
lab1:
  c += 2;
lab2:
  c++;
lab0:
  ;
}

int
main ()
{
  foo (0);
  if (c != 3)
    __builtin_abort ();
  foo (1);
  if (c != 4)
    __builtin_abort ();
  return 0;
}
