/* This issue can only exist on little-endian P8 targets, since
   the built-in functions vec_ld/vec_st can use lxvd2x/stxvd2x
   (P8 big-endian) or lxv/stxv (P9 and later) for some cases,
   those rldicr instructions fed to them are necessary.  */
/* { dg-do compile { target { powerpc_p8vector_ok && le } } } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

/* Test there are no useless instructions "rldicr x,y,0,59"
   to align the addresses for lvx/stvx.  */

extern int a, b, c;
extern vector unsigned long long ev5, ev6, ev7, ev8;
extern int dummy (vector unsigned long long);

int test_vec_ld(unsigned char *pe) {

  vector unsigned long long v1, v2, v3, v4, v9;
  vector unsigned long long v5 = ev5;
  vector unsigned long long v6 = ev6;
  vector unsigned long long v7 = ev7;
  vector unsigned long long v8 = ev8;

  unsigned char *e = pe;

  do {
    if (a) {
      v1 = __builtin_vec_ld(16, (unsigned long long *)e);
      v2 = __builtin_vec_ld(32, (unsigned long long *)e);
      v3 = __builtin_vec_ld(48, (unsigned long long *)e);
      e = e + 8;
      for (int i = 0; i < a; i++) {
        v4 = v5;
        v5 = __builtin_crypto_vpmsumd(v1, v6);
        v6 = __builtin_crypto_vpmsumd(v2, v7);
        v7 = __builtin_crypto_vpmsumd(v3, v8);
        e = e + 8;
      }
    }
    v5 = __builtin_vec_ld(16, (unsigned long long *)e);
    v6 = __builtin_vec_ld(32, (unsigned long long *)e);
    v7 = __builtin_vec_ld(48, (unsigned long long *)e);
    if (c)
      b = 1;
  } while (b);

  return dummy(v4);
}

int test_vec_st(unsigned char *pe) {

  vector unsigned long long v1, v2, v3, v4;
  vector unsigned long long v5 = ev5;
  vector unsigned long long v6 = ev6;
  vector unsigned long long v7 = ev7;
  vector unsigned long long v8 = ev8;

  unsigned char *e = pe;

  do {
    if (a) {
      __builtin_vec_st(v1, 16, (unsigned long long *)e);
      __builtin_vec_st(v2, 32, (unsigned long long *)e);
      __builtin_vec_st(v3, 48, (unsigned long long *)e);
      e = e + 8;
      for (int i = 0; i < a; i++) {
        v4 = v5;
        v5 = __builtin_crypto_vpmsumd(v1, v6);
        v6 = __builtin_crypto_vpmsumd(v2, v7);
        v7 = __builtin_crypto_vpmsumd(v3, v8);
        e = e + 8;
      }
    }
    __builtin_vec_st(v5, 16, (unsigned long long *)e);
    __builtin_vec_st(v6, 32, (unsigned long long *)e);
    __builtin_vec_st(v7, 48, (unsigned long long *)e);
    if (c)
      b = 1;
  } while (b);

  return dummy(v4);
}

/* { dg-final { scan-assembler-not {(?n)rldicr.*,0,59} } } */
