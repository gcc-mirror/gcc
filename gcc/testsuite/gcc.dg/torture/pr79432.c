/* { dg-do compile } */

int fn1 (void);
int __attribute__((returns_twice)) vfork (void);

void fn2 ()
{
  int a;
  a = fn1() + 2 + (vfork() + 1 + vfork());
}
void fn3 ()
{
  int a;
  a = fn1() + 1 + vfork();
}
void fn4 ()
{
  int a;
  a = fn1() + vfork();
}
