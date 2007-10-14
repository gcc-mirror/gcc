/* { dg-do compile } */
// PR C++/30303
// This used to ICE because we did not return NULL
// in grokfndecl when an error happened.


class A
{
  int i;
};

void foo()
{
  A();
}

A::A() {} /* { dg-error "definition of implicitly-declared" } */
