/* PR tree-optimization/113736 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=gnu23 -w" } */

#if __BITINT_MAXWIDTH__ >= 710
struct S { _BitInt(710) a; };
struct T { struct S b[4]; };

#ifdef __x86_64__
#define SEG __seg_gs
#elif defined __i386__
#define SEG __seg_fs
#else
#define SEG
#endif

void
foo (SEG struct T *p)
{
  struct S s;
  p->b[0] = s;
}

void
bar (SEG struct T *p, _BitInt(710) x, int y, double z)
{
  p->b[0].a = x + 42;
  p->b[1].a = x << y;
  p->b[2].a = x >> y;
  p->b[3].a = z;
}

int
baz (SEG struct T *p, _BitInt(710) x, _BitInt(710) y)
{
  return __builtin_add_overflow (x, y, &p->b[1].a);
}
#else
int i;
#endif
