/* PR target/128360 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection=branch -mmanual-endbr -march=x86-64" } */

static __attribute__((cf_check, noinline, noclone)) int
static_cf_target (int x)
{
  __asm__ volatile ("" : "+r" (x));
  return x + 1;
}

extern __attribute__((cf_check, noinline, noclone)) int
extern_cf_target (int x)
{
  __asm__ volatile ("" : "+r" (x));
  return x + 2;
}

int
caller (int x)
{
  return static_cf_target (x) + extern_cf_target (x);
}

/* { dg-final { scan-assembler-times "endbr(32|64)" 2 } } */
