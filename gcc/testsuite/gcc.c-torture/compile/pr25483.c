/* { dg-options "-O -fmove-loop-invariants" } */

static int mdct_win[8];
int x;

int
decode_init (double d)
{
  int j;
  for (j = 4; j; j--)
    {
      d *= 0.5;
      mdct_win[j] = (d * 3);
    }
}
