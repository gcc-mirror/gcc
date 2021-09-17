/* PR middle-end/88968 */
/* { dg-do compile } */

struct __attribute__((packed)) S {
  unsigned int a : 16;
  unsigned int b : 1;
} s;

void
foo (int y, int z)
{
  #pragma omp atomic compare
  s.a = s.a == y ? z : s.a;
}

int
bar (int y, int z)
{
  int r;
  #pragma omp atomic compare capture
  { r = s.a == y; if (r) { s.a = z; } }
  return r;
}

int
baz (int y, int z)
{
  int v;
  #pragma omp atomic compare capture
  if (s.a == y) { s.a = z; } else { v = s.a; }
  return v;
}

int
qux (int y, int z)
{
  int v;
  #pragma omp atomic compare capture
  v = s.a = s.a == y ? z : s.a;
  return v;
}
