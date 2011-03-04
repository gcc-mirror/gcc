/* { dg-do compile } */

int __attribute__ ((vector_size (32))) x;

void
foo (void)
{
  x <<= x;
}
