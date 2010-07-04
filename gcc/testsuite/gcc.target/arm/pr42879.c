/* { dg-options "-march=armv7-a -mthumb -Os" }  */
/* { dg-final { scan-assembler "lsls" } } */

struct A
{
  int v:1;
};

int bar();
int foo(struct A* p)
{
  if (p->v)
    return 1;
  return bar();
}
