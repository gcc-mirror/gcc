/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O2" } */
extern int e (void);
enum { a, b }
c (void)
{
  int d = a;
  if (e() < 0)
    d = b;
  return d;
}
/* { dg-final { scan-assembler-times "sext.w" 0 } } */
