/* { dg-do compile } */
/* { dg-options "-O2 -fconserve-stack -fdump-tree-optimized" } */
/* Check to make sure that if
   rfc4106_set_hash_subkey gets split,
   the error function does not gets split away
   from the if statement conditionalizing it.
   Checking this via the scan tree also. */
struct crypto_aes_ctx {
  char key_dec[128];
};

int rfc4106_set_hash_subkey_hash_subkey;

void __write_overflow(void)__attribute__((__error__("")));
void __write_overflow1(void);
void aes_encrypt(void*);

void fortify_panic(const char*) __attribute__((__noreturn__)) ;

char *rfc4106_set_hash_subkey(struct crypto_aes_ctx *ctx) {
  void *a = &ctx->key_dec[0];
  unsigned p_size =  __builtin_object_size(a, 0);
#ifdef __OPTIMIZE__
  if (p_size < 16) {
    __write_overflow1();
    fortify_panic(__func__);
  }
  if (p_size < 32) {
    __write_overflow();
    fortify_panic(__func__);
  }
#endif
  aes_encrypt(ctx);
  return ctx->key_dec;
}

char *(*gg)(struct crypto_aes_ctx *) = rfc4106_set_hash_subkey;

void a(void)
{
  struct crypto_aes_ctx ctx;
  rfc4106_set_hash_subkey(&ctx);
}
void b(void)
{
  struct crypto_aes_ctx ctx;
  ctx.key_dec[0] = 0;
  rfc4106_set_hash_subkey(&ctx);
}

/* This testcase should still split out one of the above basic blocks dealing
   with __write_overflow. */
/* { dg-final { scan-tree-dump-times "Function rfc4106_set_hash_subkey.part" 1 "optimized" } } */
