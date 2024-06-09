/* { dg-do compile } */

static struct A {
  int x : 4;
} a;
static int b;
int main()
{
  struct A t[2];
  t[0] = b ? t[1] : a;
  return (b ? t[1].x : 0) && 1;
}
