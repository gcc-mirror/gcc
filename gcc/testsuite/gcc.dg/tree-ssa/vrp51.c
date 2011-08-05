/* PR tree-optimization/28632 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vrp -fno-tree-ccp" } */
/* { dg-require-effective-target int32plus } */

void
v4 (unsigned a, unsigned b)
{
  if (a < 0x1000) return;
  if (a > 0x1000) return;
  if (b < 0x0110) return;
  /* constant true.  */
  if (!__builtin_constant_p ((a|b) >= 0x01000))
    __asm__("vrp.bug.always.true");
  /* VRP must not think that this is constant.  */
  if (__builtin_constant_p ((a|b) >= 0x10000))
    __asm__("vrp.bug.not.always.true");
}

void
u4 (unsigned n)
{
  if (n > 0x10111) return;
  if (n < 0x10101) return;
  /* always true.  */
  if (!__builtin_constant_p (n & 0x00100))
    __asm__("vrp.bug.always.true");
  /* VRP must not think that this is constant true.  */
  if (__builtin_constant_p (n & 0x00001))
    __asm__("vrp.bug.not.always.true");
  /* Out of range, always evaluates to constant false.  */
  if (!__builtin_constant_p (n & 0x01000))
    __asm__("vrp.bug.always.false");
}

void
u5 (unsigned n)
{
  struct s {unsigned exp:8;} x;
  x.exp = n;
  if (__builtin_constant_p(((n + 1) & 255) > 1))
    __asm__("vrp.bug.not.always.true");
}

void
v5 (int a, int b)
{
  if (a < 0x1000) return;
  if (a > 0x1000) return;
  if (b < 0x0110) return;
  /* constant true.  */
  if (!__builtin_constant_p ((a|b) >= 0x01000))
    __asm__("vrp.bug.always.true");
  /* VRP must not think that this is always true.  */
  if (__builtin_constant_p ((a|b) >= 0x10000))
    __asm__("vrp.bug.not.always.true");
}

/* { dg-final { scan-assembler-not "vrp\\\.bug\\\." } } */
