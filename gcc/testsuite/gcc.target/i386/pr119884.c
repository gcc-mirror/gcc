/* { dg-do compile } */
/* { dg-options "-O2 -fno-dse -favoid-store-forwarding" } */

typedef __attribute__((__vector_size__(64))) char V;
char c;
V v;

char
foo()
{
  v *= c;
  return v[0];
}