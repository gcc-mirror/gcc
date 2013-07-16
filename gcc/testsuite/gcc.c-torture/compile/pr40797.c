typedef struct str { short x, y;} S;

static short
bar (short ch, short sl, short sr, short tl, short tr)
{
  return 0;
}

void
foo (short ch, S *pi, short nc, S *po)
{
  short clo, chi, lo, hi;

  po->x = bar (ch, clo, chi, pi[lo].x, pi[hi].x);
  po->y = bar (ch, clo, chi, pi[lo].y, pi[hi].y);
}
