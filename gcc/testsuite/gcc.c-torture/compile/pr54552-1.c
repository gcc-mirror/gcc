void
f (void)
{
  unsigned n = 10;

  typedef double T[n];
  (double (*)[n])((unsigned char (*)[sizeof (T)]){ 0 });
}
