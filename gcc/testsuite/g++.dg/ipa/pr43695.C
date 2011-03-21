/* { dg-do compile } */
/* { dg-options "-fipa-cp -fipa-cp-clone" } */

extern void baz(int) __attribute__ ((noreturn));

struct S {
  ~S();
};

__attribute__ ((noreturn, noinline))
void bar(int i)
{
  baz(i);
}

void foo()
{
  S s;
  bar(0);
}
