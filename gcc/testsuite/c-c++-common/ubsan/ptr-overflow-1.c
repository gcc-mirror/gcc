/* PR sanitizer/80998 */
/* { dg-do run } */
/* { dg-options "-fsanitize=pointer-overflow -fno-sanitize-recover=pointer-overflow -Wall" } */

struct S { int a; int b; int c[64]; };
__attribute__((noinline, noclone)) char *f1 (char *p) { return p + 1; }
__attribute__((noinline, noclone)) char *f2 (char *p) { return p - 1; }
__attribute__((noinline, noclone)) char *f3 (char *p, int i) { return p + i; }
__attribute__((noinline, noclone)) char *f4 (char *p, int i) { return p - i; }
__attribute__((noinline, noclone)) char *f5 (char *p, unsigned long int i) { return p + i; }
__attribute__((noinline, noclone)) char *f6 (char *p, unsigned long int i) { return p - i; }
__attribute__((noinline, noclone)) int *f7 (struct S *p) { return &p->a; }
__attribute__((noinline, noclone)) int *f8 (struct S *p) { return &p->b; }
__attribute__((noinline, noclone)) int *f9 (struct S *p) { return &p->c[64]; }
__attribute__((noinline, noclone)) int *f10 (struct S *p, int i) { return &p->c[i]; }

char *volatile p;
struct S *volatile q;
char a[64];
struct S s;
int *volatile r;

int
main ()
{
  struct S t;
  p = &a[32];
  p = f1 (p);
  p = f1 (p);
  p = f2 (p);
  p = f3 (p, 1);
  p = f3 (p, -1);
  p = f3 (p, 3);
  p = f3 (p, -6);
  p = f4 (p, 1);
  p = f4 (p, -1);
  p = f4 (p, 3);
  p = f4 (p, -6);
  p = f5 (p, 1);
  p = f5 (p, 3);
  p = f6 (p, 1);
  p = f6 (p, 3);
  if (sizeof (unsigned long) >= sizeof (char *))
    {
      p = f5 (p, -1);
      p = f5 (p, -6);
      p = f6 (p, -1);
      p = f6 (p, -6);
    }
  q = &s;
  r = f7 (q);
  r = f8 (q);
  r = f9 (q);
  r = f10 (q, 0);
  r = f10 (q, 10);
  r = f10 (q, 64);
  q = &t;
  r = f7 (q);
  r = f8 (q);
  r = f9 (q);
  r = f10 (q, 0);
  r = f10 (q, 10);
  r = f10 (q, 64);
  return 0;
}
