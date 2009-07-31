struct {
}
mmaxloc0_4_i1 ()
{
  int dstride;
  int *dest;
  int rank;
  int n;
  for (n = 0; n < rank; n++)
    dest[n * dstride] = 0;
}
