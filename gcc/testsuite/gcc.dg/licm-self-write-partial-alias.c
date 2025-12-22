/* { dg-do run } */
/* { dg-options "-O2" } */

int
main (void)
{
  /* Array element shifting - partial aliasing.  */
  {
    int a[6] = {0, 0, 1, 2, 0, 0};
    unsigned char i, j;
    for (i = 1; i != 0; ++i)
      {
        for (j = 1; j <= 4; j++)
          a[j] = a[j + 1];
      }
    if (a[1] != 0)
      __builtin_abort ();
  }

  /* Memmove with overlapping regions - partial aliasing.  */
  {
    unsigned char a[6] = {0, 0, 1, 2, 0, 0};
    for (int i = 0; i < 256; i++)
      __builtin_memmove (&a[1], &a[2], 4);
    if (a[1] != 0)
      __builtin_abort ();
  }

  return 0;
}

