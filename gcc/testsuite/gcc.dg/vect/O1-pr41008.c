/* { dg-do compile } */

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


