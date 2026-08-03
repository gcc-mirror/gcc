/* { dg-do compile } */

typedef unsigned v2si __attribute__((vector_size(8)));
typedef unsigned v4si __attribute__((vector_size(16)));

unsigned bar(unsigned);

v4si x;
v2si y;

void foo(unsigned *p, unsigned i, unsigned j, unsigned k)
{
  unsigned tem0 = p[0] ^ i;
  unsigned tem1 = p[1] ^ j;
  unsigned p2 = p[2];
  unsigned p3 = p[3];
  y = (v2si) { tem0, tem1 };
  k = bar (k);
  unsigned tem2 = p2 ^ k;
  unsigned tem3 = p3 ^ k;
  x = (v4si) { tem0, tem1, tem2, tem3 };
}
