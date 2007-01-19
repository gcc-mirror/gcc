/* PR 17947 bad warning with implicit conversion and __attribute__((deprecated)) */
/* { dg-do compile } */
/* { dg-options "" } */

struct Foo
{
  operator int() __attribute__((deprecated));
};

void g(void)
{
  Foo f;
  (int)f; // { dg-warning "warning: 'Foo::operator int\\(\\)' is deprecated \\(declared at" }
}
