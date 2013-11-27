/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef int v4si __attribute__ ((vector_size (16)));

int
main (int argc, char** argv)
{
  v4si x = {0,1,2,3};
  x = (v4si) {(x)[3], (x)[2], (x)[1], (x)[0]};
  return x[4];
}
