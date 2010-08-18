/* { dg-do compile } */

void foo()
{
  void bar(int);
  void baz(int);
  void baz(void);
  void bar(void);
}
