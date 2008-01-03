/* { dg-do compile } */
/* { dg-options "-O3" } */

struct A
{
  int x[8];
};

void foo(struct A* p, long long j)
{
  int i;
  for (i = 0; i < 2; ++i)
    p->x[i+j+1] = p->x[i+j];
}
