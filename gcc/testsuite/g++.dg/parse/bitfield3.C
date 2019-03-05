/* PR c++/35320 */
/* { dg-do compile } */

typedef void (func_type)();

struct A
{
  friend func_type f : 2; /* { dg-error "20:bit-field .void f\\(\\). with non-integral type .func_type." } */
};
