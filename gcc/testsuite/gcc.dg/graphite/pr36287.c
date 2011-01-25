/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-linear" } */

int tab[2][2];

int foo ()
{
  int i, j, k;

  for (i = 0; i < 2; ++i)
    for (j = 0; j < 2; ++j)
      for (k = 0; k < 2; ++k) 
	{}

  for (i = 0; i < 2; ++i)
    for (j = 0; j < 2; ++j)
      if (i == 0)
	tab[i][j] = 0;

  return tab[0][1];
}

