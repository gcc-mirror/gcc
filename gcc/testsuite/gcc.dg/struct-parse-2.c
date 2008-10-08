/* PR c/35437 */
/* { dg-do "compile" } */

struct A
{
  int i;
  struct A a; /* { dg-error "has incomplete type" } */
};

void foo()
{
  struct A b = { 0 };
}
