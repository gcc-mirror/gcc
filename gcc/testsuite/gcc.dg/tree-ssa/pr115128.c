/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O3 -w" } */

long XXH3_len_4to8_64b_len, XXH3_len_0to16_64b___trans_tmp_3, XXH3_mix2Accs_acc,
    XXH3_64bits_internal___trans_tmp_8;
typedef unsigned long XXH3_hashLong64_f();
void *XXH3_64bits_internal_input;
int XXH3_64bits_internal___trans_tmp_1;
void XXH3_mul128_fold64();
static void XXH3_mergeAccs(unsigned long) {
  for (;;)
    XXH3_mul128_fold64(XXH3_mix2Accs_acc);
}
static __attribute__((noinline)) unsigned long
XXH3_hashLong_64b_default(void *, unsigned long len) {
  XXH3_mergeAccs(len * 7);
}
__attribute__((always_inline)) long
XXH3_64bits_internal(unsigned long len, XXH3_hashLong64_f f_hashLong) {
  if (len <= 16) {
    long keyed =
        XXH3_64bits_internal___trans_tmp_1 ^ XXH3_len_0to16_64b___trans_tmp_3;
    XXH3_mul128_fold64(keyed, XXH3_len_4to8_64b_len);
    return XXH3_64bits_internal___trans_tmp_8;
  }
  f_hashLong(XXH3_64bits_internal_input, len);
}
static void XXH_INLINE_XXH3_64bits(unsigned long len) {
  XXH3_64bits_internal(len, XXH3_hashLong_64b_default);
}
void __cmplog_rtn_hook() { XXH_INLINE_XXH3_64bits(sizeof(long)); }
