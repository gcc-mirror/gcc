/* { dg-do run { target size32plus } } */
/* { dg-options "-O3" } */

unsigned char a, b, c, d[200][200];

void abort (void);

int main ()
{
  for (; a < 200; a++)
    for (b = 0; b < 200; b++)
      if (c)
	d[a][b] = 1;

  if ((c && d[0][0] != 1) || (!c && d[0][0] != 0))
    abort ();

  return 0;
}
