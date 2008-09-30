/* PR c++/37556 */
/* { dg-do "compile" } */

struct A
{
  void foo();
};

typedef void (A::T)(); /* { dg-error "typedef name may not be a nested" } */

void bar(T); /* { dg-error "too many arguments" } */

void baz()
{
  bar(&A::foo); /* { dg-error "at this point" } */
}
