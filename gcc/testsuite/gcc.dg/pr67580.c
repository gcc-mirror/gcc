/* PR c/67580 */
/* { dg-do compile } */

struct S { int s; };
union U { int s; };
enum E { A };

void
f (void)
{
  S s; /* { dg-error "unknown type name .S.; use .struct. keyword to refer to the type" } */
  U u; /* { dg-error "unknown type name .U.; use .union. keyword to refer to the type" } */
  E e; /* { dg-error "unknown type name .E.; use .enum. keyword to refer to the type" } */
}

void
g (void)
{
  struct T { int i; };
  union V { int i; };
  enum F { J };
  T t; /* { dg-error "unknown type name .T.; use .struct. keyword to refer to the type" } */
  V v; /* { dg-error "unknown type name .V.; use .union. keyword to refer to the type" } */
  F f; /* { dg-error "unknown type name .F.; use .enum. keyword to refer to the type" } */
}
