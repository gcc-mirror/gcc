double PsyBufferUpdate(int n)
{
  if (n == 4)
    {
      _Complex double t = __builtin_cexpi(n);
      return __real t * __imag t;
    }
  return 0;
}
