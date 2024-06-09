/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx -mno-crypto" } */
/* { dg-require-effective-target powerpc_vsx } */

void use_builtins_d (__vector unsigned long long *p, __vector unsigned long long *q, __vector unsigned long long *r, __vector unsigned long long *s)
{
  p[0] = __builtin_crypto_vcipher (q[0], r[0]); /* { dg-error "'__builtin_crypto_vcipher' requires the '-mcrypto' option" } */
  p[1] = __builtin_crypto_vcipherlast (q[1], r[1]); /* { dg-error "'__builtin_crypto_vcipherlast' requires the '-mcrypto' option" } */
  p[2] = __builtin_crypto_vncipher (q[2], r[2]); /* { dg-error "'__builtin_crypto_vncipher' requires the '-mcrypto' option" } */
  p[3] = __builtin_crypto_vncipherlast (q[3], r[3]); /* { dg-error "'__builtin_crypto_vncipherlast' requires the '-mcrypto' option" } */
  p[4] = __builtin_crypto_vpermxor (q[4], r[4], s[4]);
  p[5] = __builtin_crypto_vpmsumd (q[5], r[5]);
  p[6] = __builtin_crypto_vshasigmad (q[6], 1, 15); /* { dg-error "'__builtin_crypto_vshasigmad' requires the '-mcrypto' option" } */
  p[7] = __builtin_crypto_vsbox (q[7]); /* { dg-error "'__builtin_crypto_vsbox' requires the '-mcrypto' option" } */
}

void use_builtins_w (__vector unsigned int *p, __vector unsigned int *q, __vector unsigned int *r, __vector unsigned int *s)
{
  p[0] = __builtin_crypto_vpermxor (q[0], r[0], s[0]);
  p[1] = __builtin_crypto_vpmsumw (q[1], r[1]);
  p[2] = __builtin_crypto_vshasigmaw (q[2], 1, 15); /* { dg-error "'__builtin_crypto_vshasigmaw' requires the '-mcrypto' option" } */
}

void use_builtins_h (__vector unsigned short *p, __vector unsigned short *q, __vector unsigned short *r, __vector unsigned short *s)
{
  p[0] = __builtin_crypto_vpermxor (q[0], r[0], s[0]);
  p[1] = __builtin_crypto_vpmsumh (q[1], r[1]);
}

void use_builtins_b (__vector unsigned char *p, __vector unsigned char *q, __vector unsigned char *r, __vector unsigned char *s)
{
  p[0] = __builtin_crypto_vpermxor (q[0], r[0], s[0]);
  p[1] = __builtin_crypto_vpmsumb (q[1], r[1]);
}
