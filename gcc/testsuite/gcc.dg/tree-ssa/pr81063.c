/* { dg-do run } */
/* { dg-options "-O" } */

struct A
{
  int b;
  int c:2;
};

struct B
{
  int e;
  struct A f;
} g = {0, {0, 1}}, j;

struct A *h = &g.f;

int main ()
{
  struct A k;
  struct B l = j, i = l;
  if (!i.f.b)
    k = i.f;
  *h = k;
  if (g.f.c != 0)
    __builtin_abort ();
  return 0;
}
