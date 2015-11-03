/* { dg-do run } */
/* { dg-options "-fsanitize=undefined" } */

extern void abort (void);

int
main (void)
{
  int halfmaxval = __INT_MAX__ / 2 + 1;
  int maxval = halfmaxval - 1 + halfmaxval;
  if (maxval != __INT_MAX__)
    abort ();
  return 0;
}
