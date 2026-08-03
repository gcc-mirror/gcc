/* { dg-do compile } */

unsigned bar(unsigned);

unsigned x[4];
unsigned y[2];

void foo(unsigned int *p, unsigned int i, unsigned int j, unsigned int k)
{
  unsigned int tem0 = p[0] ^ i;
  unsigned int tem1 = p[1] ^ j;
  unsigned p2 = p[2];
  unsigned p3 = p[3];
  y[0] = tem0;
  y[1] = tem1;
  k = bar (k);
  unsigned int tem2 = p2 ^ k;
  unsigned int tem3 = p3 ^ k;
  x[0] = tem0;
  x[1] = tem1;
  x[2] = tem2;
  x[3] = tem3;
}
