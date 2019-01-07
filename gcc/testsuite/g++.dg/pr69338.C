/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-Wno-pedantic" } */

struct A { char i, a[]; };

void foo()
{
  struct A a0 = { 3, "AB" };                /* { dg-error "(non-static)|(initialization)" } */
}

struct A a1 = { 3, "AB" };                  /* { dg-bogus "(non-static)|(initialization)" } */

struct A a2 = (struct A){ 3, "AB" };        /* { dg-error "(non-static)|(initialization)" } */

struct B1 {
    A a3;
    B1 (): a3 { 3, "AB" } { }               /* { dg-error "(non-static)|(initialization)" } */
} b1;

struct B2 {
    A a4;
    B2 (): a4 ((struct A){ 3, "AB" }) { }   /* { dg-error "(non-static)|(initialization)" } */
} b2;
