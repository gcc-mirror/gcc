/* PR tree-optimization/95852 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -masm=att" } */
/* { dg-final { scan-tree-dump-times " = \.MUL_OVERFLOW " 32 "optimized" } } */
/* { dg-final { scan-assembler-times "\timull\t" 32 } } */
/* In functions that return 0 on non-overflow (f2, f10, f18, f26), the overflow
   flag is propagated to the return value's PHI node in the non-call path; on
   ia32 PIC, sibcalls are not viable, so the known value of the flag can't be
   propagated to the return block, that is only duplicated in bbro, too late
   for fwprop2 or even cprop_hardreg.  */
/* { dg-final { scan-assembler-times "\tseto\t" 12 { target { ia32 && { ! nonpic } } } } } */
/* { dg-final { scan-assembler-times "\tseto\t" 8 { target { nonpic || { ! ia32 } } } } } */
/* { dg-final { scan-assembler-times "\tsetno\t" 8 } } */
/* { dg-final { scan-assembler-times "\tjn\?o\t" 16 } } */

unsigned fn (void);

int
f1 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  return x && ((int) r / (int) x) != (int) y;
}

unsigned
f2 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (x && ((int) r / (int) x) != (int) y)
    return fn ();
  return 0;
}

int
f3 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  return !x || ((int) r / (int) x) == (int) y;
}

unsigned
f4 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (!x || ((int) r / (int) x) == (int) y)
    return fn ();
  return 0;
}

int
f5 (int x, int y)
{
  int r = (unsigned) x * y;
  return x && (r / x) != y;
}

int
f6 (int x, int y)
{
  int r = (unsigned) x * y;
  if (x && (r / x) != y)
    return fn ();
  return 0;
}

int
f7 (int x, int y)
{
  int r = (unsigned) x * y;
  return !x || (r / x) == y;
}

int
f8 (int x, int y)
{
  int r = (unsigned) x * y;
  if (!x || (r / x) == y)
    return fn ();
  return 0;
}

int
f9 (unsigned x, unsigned y)
{
  unsigned r = x * y;
  return y && ((int) r / (int) y) != (int) x;
}

unsigned
f10 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (y && ((int) r / (int) y) != (int) x)
    return fn ();
  return 0;
}

int
f11 (unsigned x, unsigned y)
{
  unsigned r = x * y;
  return !y || ((int) r / (int) y) == (int) x;
}

unsigned
f12 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (!y || ((int) r / (int) y) == (int) x)
    return fn ();
  return 0;
}

int
f13 (int x, int y)
{
  int r = (unsigned) x * y;
  return y && (r / y) != x;
}

int
f14 (int x, int y)
{
  int r = (unsigned) x * y;
  if (y && (r / y) != x)
    return fn ();
  return 0;
}

int
f15 (int x, int y)
{
  int r = (unsigned) x * y;
  return !y || (r / y) == x;
}

int
f16 (int x, int y)
{
  int r = (unsigned) x * y;
  if (!y || (r / y) == x)
    return fn ();
  return 0;
}

int
f17 (unsigned x)
{
  unsigned r = x * 35U;
  return x && ((int) r / (int) x) != 35;
}

unsigned
f18 (unsigned x)
{
  unsigned int r = x * 35U;
  if (x && ((int) r / (int) x) != 35)
    return fn ();
  return 0;
}

int
f19 (unsigned x)
{
  unsigned r = x * 35U;
  return !x || ((int) r / (int) x) == 35;
}

unsigned
f20 (unsigned x)
{
  unsigned int r = x * 35U;
  if (!x || ((int) r / (int) x) == 35)
    return fn ();
  return 0;
}

int
f21 (int x)
{
  int r = (unsigned) x * 35;
  return x && (r / x) != 35;
}

int
f22 (int x)
{
  int r = (unsigned) x * 35;
  if (x && (r / x) != 35)
    return fn ();
  return 0;
}

int
f23 (int x)
{
  int r = (unsigned) x * 35;
  return !x || (r / x) == 35;
}

int
f24 (int x)
{
  int r = (unsigned) x * 35;
  if (!x || (r / x) == 35)
    return fn ();
  return 0;
}

int
f25 (unsigned x)
{
  unsigned r = x * 35U;
  return ((int) r / 35) != (int) x;
}

unsigned
f26 (unsigned x)
{
  unsigned int r = x * 35U;
  if (((int) r / 35) != (int) x)
    return fn ();
  return 0;
}

int
f27 (unsigned x)
{
  unsigned r = x * 35U;
  return !35 || ((int) r / 35) == (int) x;
}

unsigned
f28 (unsigned x)
{
  unsigned int r = x * 35U;
  if (((int) r / 35) == (int) x)
    return fn ();
  return 0;
}

int
f29 (int x)
{
  int r = (unsigned) x * 35;
  return 35 && (r / 35) != x;
}

int
f30 (int x)
{
  int r = (unsigned) x * 35;
  if ((r / 35) != x)
    return fn ();
  return 0;
}

int
f31 (int x)
{
  int r = (unsigned) x * 35;
  return (r / 35) == x;
}

int
f32 (int x)
{
  int r = (unsigned) x * 35;
  if ((r / 35) == x)
    return fn ();
  return 0;
}
