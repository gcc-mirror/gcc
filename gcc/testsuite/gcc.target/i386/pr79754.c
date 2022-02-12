/* PR target/79754 */
/* { dg-do compile } */
/* { dg-options "-Wno-psabi" } */

typedef _Decimal32 V __attribute__ ((vector_size(16)));

V fn1 (V a) { return a; }
