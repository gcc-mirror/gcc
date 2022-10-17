/* { dg-do compile } */
/* { dg-additional-options "-w -Wno-psabi" } */

int n();
typedef unsigned long V __attribute__ ((vector_size (64)));
V
foo (int i, V v)
{
  i = ((V)(V){n()})[n()];
  return v + i;
}
