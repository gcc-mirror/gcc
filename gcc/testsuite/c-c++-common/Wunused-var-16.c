/* PR c++/78949 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

typedef unsigned char V __attribute__((vector_size(16)));
V v;

void
foo ()
{
  V y = {};
  V x = {};	// { dg-bogus "set but not used" }
  y &= ~x;
  v = y;
}
