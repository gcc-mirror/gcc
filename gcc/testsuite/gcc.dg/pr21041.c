
/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fPIC" } */

struct args
{
  short int matrix[8][8];
  char **current;
};

int test (struct args *args, char *init, int a, int b)
{
  int i, j, k;

  if (!args || a > b || a < 0)
    return -1;

  for (i = 0; i < 2; i++)
    {
      char *dest = *args->current;
      char *p = dest;

      for (j = 0; j < 8; j++)
        *p++ = *init++;

      for (k = 0; k < 8; k++)
        {
          short int *blockvals = &args->matrix[k][0];
          dest[0] += blockvals[0];
          dest[1] += blockvals[1];
          dest[2] += blockvals[2];
          dest[3] += blockvals[3];
          dest[4] += blockvals[4];
          dest[5] += blockvals[5];
          dest[6] += blockvals[6];
          dest[7] += blockvals[7];
       }
    }

  return 1;
}

