/* PR debug/83621 */
/* { dg-do compile } */
/* { dg-options "-O -g" } */

typedef int __attribute__ ((__vector_size__ (64))) V;
V v;

void
foo ()
{
  V u = v >> 1;
}
