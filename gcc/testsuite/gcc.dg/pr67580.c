/* PR c/67580 */
/* { dg-do compile } */

struct S { int s; };
union U { int s; };
enum E { A };

void
f (void)
{
  S s; /* { dg-error "unknown type name" } */
/* { dg-message "use .struct. keyword to refer to the type" "" { target *-*-* } 11 } */
  U u; /* { dg-error "unknown type name" } */
/* { dg-message "use .union. keyword to refer to the type" "" { target *-*-* } 13 } */
  E e; /* { dg-error "unknown type name" } */
/* { dg-message "use .enum. keyword to refer to the type" "" { target *-*-* } 15 } */
}

void
g (void)
{
  struct T { int i; };
  union V { int i; };
  enum F { J };
  T t; /* { dg-error "unknown type name" } */
/* { dg-message "use .struct. keyword to refer to the type" "" { target *-*-* } 25 } */
  V v; /* { dg-error "unknown type name" } */
/* { dg-message "use .union. keyword to refer to the type" "" { target *-*-* } 27 } */
  F f; /* { dg-error "unknown type name" } */
/* { dg-message "use .enum. keyword to refer to the type" "" { target *-*-* } 29 } */
}
