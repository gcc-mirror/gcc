/* PR target/118940 */
/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-additional-options "-march=i386 -mregparm=3" { target ia32 } } */
/* { dg-additional-options "-fno-pie" { target pie } } */

struct A { int a[10]; };
void *b;
long c;
char d[100];
int e;
void foo (void *, void *, void *);

void
bar (int *x, int y, void *z, int w)
{
  (void) y;
  struct A *f = (void *) x;
  asm ("" : "=&D" (b), "=&c" (c), "+m" (*f) : "r" (z), "0" (x), "1" (w) : "eax");
}

void
baz (void)
{
  struct A g, h;
  struct B { int a[e]; } i;
  bar (h.a, 0, d, 0);
  bar (i.a, 0, (void *) baz, 0);
  foo (&g, &h, &i);
}
