/* Originally gcc.dg/vect/O1-pr41008.c.  */
/* { dg-options "-O1 -ftree-vectorize -fno-vect-cost-model -msve-vector-bits=256" } */

double heating[2][2];

void foo (int, int);

void map_do()
{
  int jsav, ksav, k, j;

  for(k = 0; k < 2; k++)
    for(j = 0; j < 2; j++)
      if (heating[k][j] > 0.)
        {
          jsav = j;
          ksav = k;
        }

  foo (jsav, ksav);
}
