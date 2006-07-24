/* { dg-do compile } */
/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);

int table[32][256];

int main(void)
{
  int i, j;

  for (i = 0; i < 32; i++)
    for (j = 0; j < 256; j++)
      table[i][j] = ((signed char)j) * i;

  if (table[9][132] != -1116)
    abort ();

  return 0;
}
