/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-Wno-pedantic" } */

int i;
struct A { int n, a[]; }
  a = i ? A({ 1, { 2 } })     /* { dg-error "(non-static)|(initialization)" } */
        : A({ 2, { 3, 4 } }); /* { dg-error "(non-static)|(initialization)" } */
