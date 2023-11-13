int
main(void)
{
  unsigned long L;
  double D;
  D = L = -3;
  __builtin_printf("L=%lu, D=%g\n", L, D);
  return 0;
}
