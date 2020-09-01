/* { dg-do compile } */
/* { dg-options "-mpure-code" } */

typedef int __attribute__ ((__vector_size__ (16))) V;

V v;

void
foo (void)
{
  v += (V){4095};
}
