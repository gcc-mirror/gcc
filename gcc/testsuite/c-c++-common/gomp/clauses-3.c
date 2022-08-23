struct T { int a; int *b; };
struct S { int *s; char u; struct T v; long x; };

void bar (int *);
#pragma omp declare target to (bar)
void baz (int *);
#pragma omp declare target enter (baz)

int
main ()
{
  int a[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  struct S s = { a, 5, { 6, a + 5 }, 99L };
  #pragma omp target map (s.v.a, s.u, s.x)
  ;
  #pragma omp target map (s.v.a, s.u, s.x)
  bar (&s.v.a);
  #pragma omp target map (s.v.a) map (always, to: s.u) map (s.x)
  ;
  #pragma omp target map (s.s[0]) map (s.v.b[:3])
  ;
  #pragma omp target map (s.s[0]) map (s.v.b[:3])
  baz (s.s);
  return 0;
}
