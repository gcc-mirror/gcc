/* { dg-do compile } */
/* { dg-options "-Wno-implicit-function-declaration -Wno-int-conversion -fno-builtin-free -fno-builtin-malloc" } */

void *
foo (void * p)
{
  free (p);
  return malloc (p);
}
