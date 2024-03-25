typedef struct
{
  short v, h;
} S;

S a;

void
f (S pnt)
{
  S mpnt, mtp;

  (&pnt)->v -= 1;
  mpnt = pnt;
  mtp = a;
  if (mtp.v != mpnt.v)
    {
      S tpnt;

      tpnt = mtp;
      mtp = mpnt;
      mpnt = tpnt;
    }
}
