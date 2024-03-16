/* { dg-do run } */
/* { dg-additional-options "-fno-omit-frame-pointer -w" } */

#define SIZE 8

int
main(void)
{
  int a[SIZE] = {1};
  int i;

  for (i = 1; i < SIZE; i++)
    if (a[i] != 0)
      __builtin_abort();

  __builtin_exit (0);
}
