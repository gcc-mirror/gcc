/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound -fno-sanitize-recover=vla-bound" } */

int
main (void)
{
  int x = 1;
  /* Check that the size of an array is evaluated only once.  */
  int a[++x];
  if (x != 2)
    __builtin_abort ();
  return 0;
}
