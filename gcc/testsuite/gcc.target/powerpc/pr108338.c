/* { dg-do run } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -save-temps" } */

/* Under lp64, parameter 'v' is in DI regs, then bitcast sub DI to SF. */
/* { dg-final { scan-assembler-times {\mxscvspdpn\M} 1 { target { lp64 && has_arch_pwr8 } } } } */
/* { dg-final { scan-assembler-times {\mmtvsrd\M} 1 { target { lp64 && has_arch_pwr8 } } } } */
/* { dg-final { scan-assembler-times {\mrldicr\M} 1 { target { lp64 && has_arch_pwr8 } } } } */

struct di_sf_sf
{
  float f1; float f2; long long l;
};

float __attribute__ ((noipa))
sf_from_high32bit_di (struct di_sf_sf v)
{
#ifdef __LITTLE_ENDIAN__
  return v.f2;
#else
  return v.f1;
#endif
}

int main()
{
  struct di_sf_sf v;
  v.f1 = v.f2 = 0.0f;
#ifdef __LITTLE_ENDIAN__
  v.f2 = 2.0f;
#else
  v.f1 = 2.0f;
#endif
  if (sf_from_high32bit_di (v) != 2.0f)
    __builtin_abort ();
  return 0;
}
