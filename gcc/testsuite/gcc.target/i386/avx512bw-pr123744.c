/* PR middle-end/123744 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512bw" } */

int c, d, e;
long long a;
signed char *b;
short f;

void
foo (short i[][7][7], char j[][7][7])
{
  for (int h = 0; h < 128; h++)
    {
      c = e ? b[1] : 0;
      if (a)
	d = (f ? j[0][0][1] : i[4][4][1]) / 12;
    }
}
