/* PR c++/37556 */
/* { dg-do compile } */

struct A
{
  void foo();
};

typedef void (A::T)(); /* { dg-error "15:typedef name may not be a nested" } */

void bar(T);

void baz()
{
  bar(&A::foo);
}
