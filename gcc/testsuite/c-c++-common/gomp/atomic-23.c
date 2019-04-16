/* PR middle-end/88968 */
/* { dg-do compile } */

struct __attribute__((packed)) S {
  unsigned int a : 16;
  unsigned int b : 1;
} s;

void
f1 (void)
{
#pragma omp atomic
  ++s.a;
}

int
f2 (void)
{
  int r;
#pragma omp atomic capture
  {
    r = s.a;
    s.a = 0;
  }
  return r;
}

int
f3 (void)
{
  int r;
#pragma omp atomic capture
  {
    r = s.a;
    s.a = s.a + 32;
  }
  return r;
}

int
f4 (void)
{
  int r;
#pragma omp atomic capture
  r = s.a = s.a + 32;
  return r;
}
