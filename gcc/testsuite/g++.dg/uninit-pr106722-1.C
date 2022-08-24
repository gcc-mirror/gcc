// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -Wmaybe-uninitialized --param logical-op-non-short-circuit=0" }
long pow2p_hwi_x;
bool exact_log2___trans_tmp_5, exact_log2___trans_tmp_4;
int exact_log2(long x) {
  exact_log2___trans_tmp_5 = pow2p_hwi_x && exact_log2___trans_tmp_4;
  return exact_log2___trans_tmp_5 ? x : 1;
}
enum signop {};
template <typename T1, typename T2> void rshift(T1, T2, signop);
struct generic_wide_int {
  template <typename T> generic_wide_int(T);
};
template <unsigned N, typename> struct poly_int_pod {
  bool is_constant() const;
  template <typename T> bool is_constant(T *) const;
  int coeffs[N];
};
template <unsigned N, typename C>
template <typename T>
bool poly_int_pod<N, C>::is_constant(T *const_value) const {
  if (is_constant()) {
    *const_value = coeffs[0];
    return true;
  }
  return false;
}
struct poly_int : poly_int_pod<1, int> {
  template <typename C0> poly_int(C0);
};
enum tree_code_class {} tree_code_type;
void tree_class_check_failed(int *, tree_code_class, char *, int, char *)
    __attribute__((__noreturn__));
int tree_class_check___t, tree_class_check___l,
    vect_gen_vector_loop_niters_loop_vinfo;
char tree_class_check___f, tree_class_check___g;
tree_code_class tree_class_check___class;
int *tree_class_check() {
  if (tree_code_type)
    tree_class_check_failed(&tree_class_check___t, tree_class_check___class,
                            &tree_class_check___f, tree_class_check___l,
                            &tree_class_check___g);
  return &tree_class_check___t;
}
int *build_int_cst(int, long);
bool is_gimple_val(int);
void force_gimple_operand(int, int *, bool, int);
void vect_gen_vector_loop_niters(bool niters_no_overflow) {
  poly_int vf(vect_gen_vector_loop_niters_loop_vinfo);
  int *log_vf = nullptr;
  long const_vf;
  if (vf.is_constant(&const_vf))
    log_vf = build_int_cst(0, 0);
  if (is_gimple_val(0)) {
    int stmts;
    force_gimple_operand(0, &stmts, true, 0);
    if (stmts && log_vf)
      if (niters_no_overflow) {
        generic_wide_int __trans_tmp_1(tree_class_check());
        int __trans_tmp_2 = exact_log2(const_vf); // { dg-bogus "uninitialized" }
        rshift(__trans_tmp_1, __trans_tmp_2, (signop)0);
      }
  }
}
