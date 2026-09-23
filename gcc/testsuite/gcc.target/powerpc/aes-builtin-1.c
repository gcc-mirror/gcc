/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

void
aes_enc_pair_0 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_encrypt_paired (t, k, 0);
  *res = c;
}
void
aes_enc_pair_1 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_encrypt_paired (t, k, 1);
  *res = c;
}
void
aes_enc_pair_2 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_encrypt_paired (t, k, 2);
  *res = c;
}
void
aes_enc_pair_128 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes128_encrypt_paired (t, k);
  *res = c;
}
void
aes_enc_pair_192 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes192_encrypt_paired (t, k);
  *res = c;
}
void
aes_enc_pair_256 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes256_encrypt_paired (t, k);
  *res = c;
}
void
aes_dec_pair_0 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_decrypt_paired (t, k, 0);
  *res = c;
}
void
aes_dec_pair_1 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_decrypt_paired (t, k, 1);
  *res = c;
}
void
aes_dec_pair_2 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_decrypt_paired (t, k, 2);
  *res = c;
}
void
aes_dec_pair_128 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes128_decrypt_paired (t, k);
  *res = c;
}
void
aes_dec_pair_192 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes192_decrypt_paired (t, k);
  *res = c;
}
void
aes_dec_pair_256 (__vector_pair *text, __vector_pair *key, __vector_pair *res)
{
  __vector_pair t = *text;
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes256_decrypt_paired (t, k);
  *res = c;
}
void
aes_genlastkey_paired_0 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_genlastkey_paired (k, 0);
  *res = c;
}
void
aes_genlastkey_paired_1 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_genlastkey_paired (k, 1);
  *res = c;
}
void
aes_genlastkey_paired_2 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes_genlastkey_paired (k, 2);
  *res = c;
}
void
aes_genlastkey_paired_128 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes128_genlastkey_paired (k);
  *res = c;
}
void
aes_genlastkey_paired_192 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes192_genlastkey_paired (k);
  *res = c;
}
void
aes_genlastkey_paired_256 (__vector_pair *key, __vector_pair *res)
{
  __vector_pair k = *key;
  __vector_pair c = __builtin_aes256_genlastkey_paired (k);
  *res = c;
}


/* { dg-final { scan-assembler-times {\mxxaesencp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxaes128encp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes192encp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes256encp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaesdecp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxaes128decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes192decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes256decp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaesgenlkp\M} 3 } } */
/* { dg-final { scan-assembler-times {\mxxaes128genlkp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes192genlkp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxaes256genlkp\M} 1 } } */
