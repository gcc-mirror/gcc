/* { dg-skip-if "" { ! aarch64*-*-* } } */
static inline long long
rwsr (void)
{
  long long a  =  __builtin_aarch64_rsr64 ("trcseqstr");
  __builtin_aarch64_wsr64 ("trcseqstr", a + 1);
  a = __builtin_aarch64_rsr64 ("trcseqstr");
  return a;
}

