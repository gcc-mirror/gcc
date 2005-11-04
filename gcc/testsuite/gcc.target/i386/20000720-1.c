/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mpreferred-stack-boundary=2 -march=i586 -O2 -fomit-frame-pointer" } */

extern void *foo(void *a, const void *b, unsigned c);

extern inline void *
bar(void *a, const void *b, unsigned c)
{
  int d0, d1, d2;
  __asm__ __volatile__(
    "" :
    "=&c" (d0), "=&D" (d1), "=&S" (d2) :
    "0" (c/4), "q" (c), "1" (a), "2" (b) :
    "memory");
  return a;
}

typedef struct {
  unsigned char a;
  unsigned b : 2;
  unsigned c : 4;
  unsigned d : 2;
} *baz;

static int
dead(unsigned short *v, char *w, unsigned char *x, int y, int z)
{
  int i = 0;
  unsigned short j = *v;

  while (y > 0) {
    ((baz)x)->a = j;
    ((baz)x)->b = 0;
    ((baz)x)->c = 0;
    ((baz)x)->d = 0;
    __builtin_constant_p(i) ? foo(x, w, i) : bar(x, w, i);
  }
  return z - y;
}
