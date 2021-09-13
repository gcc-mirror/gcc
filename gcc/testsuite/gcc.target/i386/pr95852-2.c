/* PR tree-optimization/95852 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -masm=att" } */
/* { dg-final { scan-tree-dump-times " = \.MUL_OVERFLOW " 32 "optimized" } } */
/* { dg-final { scan-assembler-times "\tmull\t" 32 } } */
/* { dg-final { scan-assembler-times "\tseto\t" 8 } } */
/* { dg-final { scan-assembler-times "\tsetno\t" 8 } } */
/* { dg-final { scan-assembler-times "\tjn\?o\t" 16 } } */

unsigned fn (void);

int
f1 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  return x && (r / x) != y;
}

unsigned
f2 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (x && (r / x) != y)
    return fn ();
  return 0;
}

int
f3 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  return !x || (r / x) == y;
}

unsigned
f4 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (!x || (r / x) == y)
    return fn ();
  return 0;
}

int
f5 (int x, int y)
{
  int r = (unsigned) x * y;
  return x && ((unsigned) r / x) != (unsigned) y;
}

int
f6 (int x, int y)
{
  int r = (unsigned) x * y;
  if (x && ((unsigned) r / x) != (unsigned) y)
    return fn ();
  return 0;
}

int
f7 (int x, int y)
{
  int r = (unsigned) x * y;
  return !x || ((unsigned) r / x) == (unsigned) y;
}

int
f8 (int x, int y)
{
  int r = (unsigned) x * y;
  if (!x || ((unsigned) r / x) == (unsigned) y)
    return fn ();
  return 0;
}

int
f9 (unsigned x, unsigned y)
{
  unsigned r = x * y;
  return y && (r / y) != x;
}

unsigned
f10 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (y && (r / y) != x)
    return fn ();
  return 0;
}

int
f11 (unsigned x, unsigned y)
{
  unsigned r = x * y;
  return !y || (r / y) == x;
}

unsigned
f12 (unsigned x, unsigned y)
{
  unsigned int r = x * y;
  if (!y || (r / y) == x)
    return fn ();
  return 0;
}

int
f13 (int x, int y)
{
  int r = (unsigned) x * y;
  return y && ((unsigned) r / y) != (unsigned) x;
}

int
f14 (int x, int y)
{
  int r = (unsigned) x * y;
  if (y && ((unsigned) r / y) != (unsigned) x)
    return fn ();
  return 0;
}

int
f15 (int x, int y)
{
  int r = (unsigned) x * y;
  return !y || ((unsigned) r / y) == (unsigned) x;
}

int
f16 (int x, int y)
{
  int r = (unsigned) x * y;
  if (!y || ((unsigned) r / y) == (unsigned) x)
    return fn ();
  return 0;
}

int
f17 (unsigned x)
{
  unsigned r = x * 35U;
  return x && (r / x) != 35U;
}

unsigned
f18 (unsigned x)
{
  unsigned int r = x * 35U;
  if (x && (r / x) != 35U)
    return fn ();
  return 0;
}

int
f19 (unsigned x)
{
  unsigned r = x * 35U;
  return !x || (r / x) == 35U;
}

unsigned
f20 (unsigned x)
{
  unsigned int r = x * 35U;
  if (!x || (r / x) == 35U)
    return fn ();
  return 0;
}

int
f21 (int x)
{
  int r = (unsigned) x * 35;
  return x && ((unsigned) r / x) != 35U;
}

int
f22 (int x)
{
  int r = (unsigned) x * 35;
  if (x && ((unsigned) r / x) != 35U)
    return fn ();
  return 0;
}

int
f23 (int x)
{
  int r = (unsigned) x * 35;
  return !x || ((unsigned) r / x) == 35U;
}

int
f24 (int x)
{
  int r = (unsigned) x * 35;
  if (!x || ((unsigned) r / x) == 35U)
    return fn ();
  return 0;
}

int
f25 (unsigned x)
{
  unsigned r = x * 35U;
  return (r / 35U) != x;
}

unsigned
f26 (unsigned x)
{
  unsigned int r = x * 35U;
  if ((r / 35U) != x)
    return fn ();
  return 0;
}

int
f27 (unsigned x)
{
  unsigned r = x * 35U;
  return !35U || (r / 35U) == x;
}

unsigned
f28 (unsigned x)
{
  unsigned int r = x * 35U;
  if ((r / 35U) == x)
    return fn ();
  return 0;
}

int
f29 (int x)
{
  int r = (unsigned) x * 35;
  return 35 && ((unsigned) r / 35) != (unsigned) x;
}

int
f30 (int x)
{
  int r = (unsigned) x * 35;
  if (((unsigned) r / 35) != (unsigned) x)
    return fn ();
  return 0;
}

int
f31 (int x)
{
  int r = (unsigned) x * 35;
  return ((unsigned) r / 35) == (unsigned) x;
}

int
f32 (int x)
{
  int r = (unsigned) x * 35;
  if (((unsigned) r / 35) == (unsigned) x)
    return fn ();
  return 0;
}
