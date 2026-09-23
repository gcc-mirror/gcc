/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

void
aes_dec_pair (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair lk = __builtin_aes_genlastkey_paired (k, 0);
  __vector_pair c = __builtin_aes_decrypt_paired (t, lk, 0);
  lk = __builtin_aes_genlastkey_paired (k, 1);
  c = __builtin_aes_decrypt_paired (c, lk, 1);
  lk = __builtin_aes_genlastkey_paired (k, 2);
  c = __builtin_aes_decrypt_paired (c, lk, 2);
  *res = c;
}
void
aes_dec_pair_128 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair lk = __builtin_aes128_genlastkey_paired (k);
  __vector_pair c = __builtin_aes128_decrypt_paired (t, lk);
  *res = c;
}
void
aes_dec_pair_192 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair lk = __builtin_aes192_genlastkey_paired (k);
  __vector_pair c = __builtin_aes192_decrypt_paired (t, lk);
  *res = c;
}
void
aes_dec_pair_256 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair lk = __builtin_aes256_genlastkey_paired (k);
  __vector_pair c = __builtin_aes256_decrypt_paired (t, lk);
  *res = c;
}

/* { dg-final { scan-assembler-times {\mxxaesdecp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxaes128decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes192decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes256decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaesgenlkp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxaes128genlkp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes192genlkp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes256genlkp\M} 1 } } */