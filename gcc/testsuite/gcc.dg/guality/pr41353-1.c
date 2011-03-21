/* PR debug/41353 */
/* { dg-do run } */
/* { dg-options "-g" } */

int vari __attribute__((used)) = 17, varj;

__attribute__((noinline)) int
f1 (void)
{
  /* { dg-final { gdb-test 17 "vari" "17" } } */
  int vari1 = 2 * vari; /* { dg-final { gdb-test 17 "vari1" "2 * 17" } } */
  int vari2 = 3 * vari; /* { dg-final { gdb-test 17 "vari2" "3 * 17" } } */
  int vari3 = 2 * vari; /* { dg-final { gdb-test 17 "vari3" "2 * 17" } } */
  int vari4 = 3 * vari; /* { dg-final { gdb-test 17 "vari4" "3 * 17" } } */
  int vari5 = 4 * vari; /* { dg-final { gdb-test 17 "vari5" "4 * 17" } } */
  int vari6 = 5 * vari; /* { dg-final { gdb-test 17 "vari6" "5 * 17" } } */
  return varj;
}

__attribute__((noinline)) int
f2 (int i, int j)
{
  j += i;
  /* { dg-final { gdb-test 28 "i" "37" } } */
  /* { dg-final { gdb-test 28 "j" "28 + 37" { xfail *-*-* } } } */
  int i1 = 2 * i; /* { dg-final { gdb-test 28 "i1" "2 * 37" } } */
  int i2 = 3 * i; /* { dg-final { gdb-test 28 "i2" "3 * 37" } } */
  return j;
}

__attribute__((noinline)) int
f3 (int i)
{
  asm volatile ("" : "+r" (i));
  /* { dg-final { gdb-test 39 "i" "12" } } */
  int i1 = 2 * i; /* { dg-final { gdb-test 39 "i1" "2 * 12" } } */
  int i2 = 2 * i; /* { dg-final { gdb-test 39 "i2" "2 * 12" } } */
  int i3 = 3 * i; /* { dg-final { gdb-test 39 "i3" "3 * 12" } } */
  return i;
}

int (*volatile fnp1) (void) = f1;
int (*volatile fnp2) (int, int) = f2;
int (*volatile fnp3) (int) = f3;

int
main (int argc, char *argv[])
{
  asm volatile ("" : : "r" (&fnp1) : "memory");
  asm volatile ("" : : "r" (&fnp2) : "memory");
  asm volatile ("" : : "r" (&fnp3) : "memory");
  fnp1 ();
  fnp2 (37, 28);
  fnp3 (12);
  return 0;
}
