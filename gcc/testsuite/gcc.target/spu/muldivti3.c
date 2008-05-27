/* { dg-do run } */
/* { dg-options "-std=c99" } */
#include <stdlib.h>
typedef unsigned int uqword __attribute__((mode(TI)));
typedef int qword __attribute__((mode(TI)));

typedef union
{
  uqword                uq;
  qword                 q;
  unsigned long long    ull[2];
} u;

int main(void)
{
  uqword e, f;
  qword g, h;

  e = 0x1111111111111111ULL;
  f = 0xFULL;
  g = 0x0000000000111100ULL;
  h = 0x0000000000000000ULL;

  u m, n, o, p, q;

  m.ull[0] = f;
  m.ull[1] = e;
  n.ull[0] = h;
  n.ull[1] = g;

  /* __multi3  */
  o.q = m.q * n.q;

  o.q = o.q + n.q + 0x1110FF;
  /* __udivti3, __umodti3  */
  p.uq = o.uq / n.uq;
  q.uq = o.uq % n.uq;
  if (p.uq != (m.uq+1)) abort();
  if (q.uq != 0x1110FF) abort();
  /* __divti3, __modti3  */
  p.q = -o.q / n.q;
  q.q = -o.q % n.q;
  if ((-p.q * n.q - q.q) != o.q) abort();

  return 0;
}
