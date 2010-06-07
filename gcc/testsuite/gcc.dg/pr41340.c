/* PR debug/41340 */
/* { dg-do compile } */
/* { dg-options "-O3 -g -fcompare-debug" } */
/* { dg-options "-O3 -g -fcompare-debug -march=i686" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-skip-if "no long pointers" {  { ! ilp32 } && { ! lp64 } } } */

typedef struct { int t; } *T;
struct S1 { unsigned s1; };
struct S2 { struct S1 s2; };
struct S3 { unsigned s3; struct S2 **s4; };
struct S5 { struct S2 *s5; };

__extension__ typedef __INTPTR_TYPE__ ssize_t;

extern void fn0 (void) __attribute__ ((__noreturn__));
T fn6 (struct S3);
void fn7 (void);

static inline __attribute__((always_inline)) int
fn1 (const struct S1 *x)
{
  return x->s1;
}

static inline __attribute__((always_inline)) int
fn2 (const struct S1 *x, unsigned y)
{
  if (y >= x->s1)
    fn0 ();
  return 0;
}

static inline __attribute__((always_inline)) int
fn3 (struct S3 x)
{
  return (x.s3 == fn1 (*x.s4 ? &(*x.s4)->s2 : 0));
}

static inline __attribute__((always_inline)) int
fn4 (struct S3 x)
{
  return fn2 (&(*x.s4)->s2, x.s3);
}

int
fn5 (struct S3 x, T *y)
{
  if (!fn3 (x))
    {
      *y = (T) (ssize_t) fn4 (x);
      return 1;
    }
  return 0;
}

void
test (struct S5 *x)
{
  struct S3 a;
  T b;
  unsigned char c = 0;
  a.s4 = &x->s5;
  while (fn5 (a, &b))
    if (!(b->t & 8))
      c = 1;
  a.s4 = &x->s5;
  while ((b = fn6 (a)))
    ;
  if (!c)
    fn7 ();
}
