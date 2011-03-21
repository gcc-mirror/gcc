/* PR c++/35320 */
/* { dg-do compile } */

typedef void (func_type)();

struct A
{
  friend func_type f : 2; /* { dg-error "with non-integral type" } */
};
