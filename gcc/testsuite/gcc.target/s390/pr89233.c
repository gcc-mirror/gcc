/* { dg-do compile } */
/* { dg-options "-march=z13 -O1" } */

typedef int v4si __attribute__ ((vector_size (16)));

int
f ()
{
  v4si x = {0, 1, 2, 3};
  return x[4];
}
