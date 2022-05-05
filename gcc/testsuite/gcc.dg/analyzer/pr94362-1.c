/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* TODO: remove the need for -Wno-analyzer-too-complex.  */

typedef struct evp_pkey_asn1_method_st EVP_PKEY_ASN1_METHOD;
typedef struct engine_st ENGINE;
struct stack_st_EVP_PKEY_ASN1_METHOD;
struct evp_pkey_asn1_method_st {
  unsigned long pkey_flags;
};

const EVP_PKEY_ASN1_METHOD *ENGINE_pkey_asn1_find_str(ENGINE **pe,
                                                      const char *str, int len);
extern int
sk_EVP_PKEY_ASN1_METHOD_num(const struct stack_st_EVP_PKEY_ASN1_METHOD *sk);
extern const EVP_PKEY_ASN1_METHOD *
sk_EVP_PKEY_ASN1_METHOD_value(const struct stack_st_EVP_PKEY_ASN1_METHOD *sk,
                              int idx);
extern const EVP_PKEY_ASN1_METHOD hmac_asn1_meth;
static const EVP_PKEY_ASN1_METHOD *standard_methods[] = {&hmac_asn1_meth};
static struct stack_st_EVP_PKEY_ASN1_METHOD *app_methods = ((void *)0);

int EVP_PKEY_asn1_get_count(void) {
  int num = (sizeof(standard_methods) / sizeof((standard_methods)[0]));
  if (app_methods)
    num += sk_EVP_PKEY_ASN1_METHOD_num(app_methods);
  return num;
}

const EVP_PKEY_ASN1_METHOD *EVP_PKEY_asn1_get0(int idx) {
  int num = (sizeof(standard_methods) / sizeof((standard_methods)[0]));
  if (idx < 0)
    return ((void *)0);
  if (idx < num)
    return standard_methods[idx];
  idx -= num;
  return sk_EVP_PKEY_ASN1_METHOD_value(app_methods, idx);
}

const EVP_PKEY_ASN1_METHOD *EVP_PKEY_asn1_find_str(ENGINE **pe, const char *str,
                                                   int len) {
  int i;
  const EVP_PKEY_ASN1_METHOD *ameth = ((void *)0);

  if (pe) {
    ENGINE *e;
    ameth = ENGINE_pkey_asn1_find_str(&e, str, len);
    if (ameth) {
      *pe = e;
      return ameth;
    }
    *pe = ((void *)0);
  }
  for (i = EVP_PKEY_asn1_get_count(); i-- > 0;) {
    ameth = EVP_PKEY_asn1_get0(i);
    if (ameth->pkey_flags & 0x1)
      continue;
    return ameth;
  }
  return ((void *)0);
}
