/* { dg-do compile } */
/* { dg-options "-std=gnu90" } */

typedef int V __attribute__ ((vector_size(4)));
void fn1 ()
{
  (V){(1,0)}[0] = 0;
}
