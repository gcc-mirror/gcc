void
mt (double);

void
nm (void)
{
  double ao = 0.0;
  long int es = -1;

  mt (ao);
  ++ao;
  mt (ao);
  mt (*(double *) &es);
}
