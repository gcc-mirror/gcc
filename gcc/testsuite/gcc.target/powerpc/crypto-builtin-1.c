/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#include <altivec.h>
typedef vector unsigned long long	crypto_t;
typedef vector unsigned long long	v2di_t;
typedef vector unsigned int		v4si_t;
typedef vector unsigned short		v8hi_t;
typedef vector unsigned char		v16qi_t;

crypto_t crypto1 (crypto_t a)
{
  return __builtin_crypto_vsbox (a);
}

v16qi_t crypto1_be (v16qi_t a)
{
  return vec_sbox_be (a);
}

crypto_t crypto2 (crypto_t a, crypto_t b)
{
  return __builtin_crypto_vcipher (a, b);
}

v16qi_t crypto2_be (v16qi_t a, v16qi_t b)
{
  return vec_cipher_be (a, b);
}

crypto_t crypto3 (crypto_t a, crypto_t b)
{
  return __builtin_crypto_vcipherlast (a, b);
}

v16qi_t crypto3_be (v16qi_t a, v16qi_t b)
{
  return vec_cipherlast_be (a, b);
}

crypto_t crypto4 (crypto_t a, crypto_t b)
{
  return __builtin_crypto_vncipher (a, b);
}

v16qi_t crypto4_be (v16qi_t a, v16qi_t b)
{
  return vec_ncipher_be (a, b);
}

crypto_t crypto5 (crypto_t a, crypto_t b)
{
  return __builtin_crypto_vncipherlast (a, b);
}

v16qi_t crypto5_be (v16qi_t a, v16qi_t b)
{
  return vec_ncipherlast_be (a, b);
}

v16qi_t crypto6a (v16qi_t a, v16qi_t b, v16qi_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v8hi_t crypto6b (v8hi_t a, v8hi_t b, v8hi_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v4si_t crypto6c (v4si_t a, v4si_t b, v4si_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v2di_t crypto6d (v2di_t a, v2di_t b, v2di_t c)
{
  return __builtin_crypto_vpermxor (a, b, c);
}

v16qi_t crypto7a (v16qi_t a, v16qi_t b)
{
  return __builtin_crypto_vpmsumb (a, b);
}

v16qi_t crypto7b (v16qi_t a, v16qi_t b)
{
  return __builtin_crypto_vpmsum (a, b);
}

v8hi_t crypto7c (v8hi_t a, v8hi_t b)
{
  return __builtin_crypto_vpmsumh (a, b);
}

v8hi_t crypto7d (v8hi_t a, v8hi_t b)
{
  return __builtin_crypto_vpmsum (a, b);
}

v4si_t crypto7e (v4si_t a, v4si_t b)
{
  return __builtin_crypto_vpmsumw (a, b);
}

v4si_t crypto7f (v4si_t a, v4si_t b)
{
  return __builtin_crypto_vpmsum (a, b);
}

v2di_t crypto7g (v2di_t a, v2di_t b)
{
  return __builtin_crypto_vpmsumd (a, b);
}

v2di_t crypto7h (v2di_t a, v2di_t b)
{
  return __builtin_crypto_vpmsum (a, b);
}

v2di_t crypto8a (v2di_t a)
{
  return __builtin_crypto_vshasigmad (a, 0, 8);
}

v2di_t crypto8b (v2di_t a)
{
  return __builtin_crypto_vshasigma (a, 0, 8);
}

v4si_t crypto8c (v4si_t a)
{
  return __builtin_crypto_vshasigmaw (a, 1, 15);
}

v4si_t crypto8d (v4si_t a)
{
  return __builtin_crypto_vshasigma (a, 1, 15);
}

/* Note space is used after the instruction so that vcipherlast does not match
   vcipher.  */
/* { dg-final { scan-assembler-times "vcipher "      2 } } */
/* { dg-final { scan-assembler-times "vcipherlast "  2 } } */
/* { dg-final { scan-assembler-times "vncipher "     2 } } */
/* { dg-final { scan-assembler-times "vncipherlast " 2 } } */
/* { dg-final { scan-assembler-times "vpermxor "     4 } } */
/* { dg-final { scan-assembler-times "vpmsumb "      2 } } */
/* { dg-final { scan-assembler-times "vpmsumd "      2 } } */
/* { dg-final { scan-assembler-times "vpmsumh "      2 } } */
/* { dg-final { scan-assembler-times "vpmsumw "      2 } } */
/* { dg-final { scan-assembler-times "vsbox "        2 } } */
/* { dg-final { scan-assembler-times "vshasigmad "   2 } } */
/* { dg-final { scan-assembler-times "vshasigmaw "   2 } } */
