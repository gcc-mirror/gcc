/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-O2 -fstack-clash-protection -march=rv64gcv_zvl256b -mabi=lp64d" } */

int pk_gen_i, pk_gen_j;
long pk_gen_buf[4];
long pk_gen_t;

void vec_mul(long *, long *, long *);
void uint64_is_zero_declassify(long);

void pk_gen() {
  long consts[128][13], prod[128][13];

  vec_mul(prod[pk_gen_j], prod[pk_gen_j], consts[pk_gen_j]);

  for (; pk_gen_i;) {
    for (; pk_gen_j; pk_gen_j++)
      pk_gen_t |= pk_gen_buf[pk_gen_j];

    uint64_is_zero_declassify(pk_gen_t);
  }
}
