/* PR debug/41353 */
/* { dg-do run } */
/* { dg-options "-g" } */

int varh;
int vari __attribute__((used)) = 17, varj;

__attribute__((noinline)) int
f1 (void)
{
  int vari1 = 2 * vari; /* { dg-final { gdb-test 13 "vari1" "2 * 17" } } */
  int vari2 = 3 * vari; /* { dg-final { gdb-test 13 "vari2" "3 * 17" } } */
  return varj;
}

int (*volatile fnp1) (void) = f1;

int
main (int argc, char *argv[])
{
  asm volatile ("" : : "r" (&fnp1) : "memory");
  fnp1 ();
  return 0;
}
