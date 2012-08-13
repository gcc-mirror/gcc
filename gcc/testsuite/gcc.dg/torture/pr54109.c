/* { dg-do compile } */

typedef struct
{
  unsigned long bits[4];
} nodemask_t;

struct cpuset
{
  long flags;
  nodemask_t mems_allowed;
  struct cpuset *parent;
} b;

void func1(unsigned long *p1, int p2)
{
  p1[p2 - 1] = 0;
}

void func2(nodemask_t *p1, int p2)
{
  func1(p1->bits, p2);
}

void func3(void)
{
  /* This accesses b.flags.  */
  func2(&b.mems_allowed, 0);
}
