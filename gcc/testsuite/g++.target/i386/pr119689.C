// { dg-do compile }
// { dg-options "-O2 -fcompare-debug" }
// { dg-additional-options "-march=i586 -mtune=generic" { target ia32 } }
// { dg-additional-options "-fPIC" { target { fpic } } }

enum gimple_code { GIMPLE_ASSIGN, GIMPLE_RETURN };
bool is_gimple_call();
int m_sig, m_exp, sreal_new_exp;
struct sreal {
  sreal(long long sig) {
    long long __trans_tmp_6 = sig >= 0 ? sig : -(unsigned long long)sig;
    sig = __trans_tmp_6 <<= sreal_new_exp -= m_exp = __trans_tmp_6;
    m_sig = sig;
  }
  void operator/(sreal);
};
struct ipa_predicate {
  ipa_predicate(bool = true);
  void operator&=(ipa_predicate);
  void operator&(ipa_predicate);
};
void add_condition();
gimple_code eliminated_by_inlining_prob_code;
static int eliminated_by_inlining_prob() {
  switch (eliminated_by_inlining_prob_code) {
  case GIMPLE_RETURN:
    return 2;
  case GIMPLE_ASSIGN:
    return 1;
  }
  return 0;
}
void fp_expression_p() {
  ipa_predicate bb_predicate;
  for (;;) {
    int prob = eliminated_by_inlining_prob();
    ipa_predicate sra_predicate;
    sra_predicate &= add_condition;
    if (is_gimple_call())
      sreal(prob) / 2;
    if (prob != 2)
      bb_predicate & sra_predicate;
  }
}
