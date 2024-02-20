/* { dg-do compile } */
/* { dg-options "-O" } */

const short __attribute__((vector_size(16))) y = { 0, 1, 2, 3, 4, 5, 6, 7 };

int
main (int argc, short *argv[])
{
  int i = argc;
  y[i] = 7 - i; /* { dg-error "read-only" } */
  return 0;
}
