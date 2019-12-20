/* PR target/92615 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void *a;
long long b;
char c;

void
foo (void)
{
  void *p;
  long long q;
  char r;
  __asm__ ("" : : "r" (&p), "r" (&q), "r" (&r));
  __asm__ ("" : "=@cca" (p));
  a = p;
  __asm__ ("" : "=@cca" (q));
  b = q;
  __asm__ ("" : "=@cca" (r));
  c = r;
  __asm__ ("" : : "r" (&p), "r" (&q), "r" (&r));
}

void
bar (void)
{
  void *p;
  long long q;
  char r;
  __asm__ ("" : "=@cca" (p));
  a = p;
  __asm__ ("" : "=@cca" (q));
  b = q;
  __asm__ ("" : "=@cca" (r));
  c = r;
  __asm__ ("" : : "r" (p), "A" (q), "q" (r));
}

void
baz (void)
{
  void *p = (void *) &p;
  __asm__ __volatile__ ("" : "=@ccng" (p) : "r" (1));
}
