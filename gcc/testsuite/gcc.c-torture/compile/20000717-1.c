short
inner_product (short *a, short *b)
{
  int i;
  short sum = 0;

  for (i = 9; i >= 0; i--)
    sum += (*a++) * (*b++);

  return sum;
}
