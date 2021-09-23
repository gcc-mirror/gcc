/* Check operator with __attribute__((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

struct Foo
{
  operator int() __attribute__((unavailable));
};

void g(void)
{
  Foo f;
  (int)f; // { dg-error "'Foo::operator int\\(\\)' is unavailable" }
}
