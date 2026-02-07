// { dg-additional-options "-O3" }

void *operator new(unsigned long, void *);
int to_constant();
int vec_copy_construct_n, vec_stmts_size, vectorizable_load_vec_stmt,
    vectorizable_load_offvar;
enum tree_code { POINTER_PLUS_EXPR };
void gimple_build_assign(int, tree_code, int, int);
int cse_and_gimplify_to_preheader(int, int);
void vectorizable_load() {
  int i, stride_step, const_nunits = to_constant();
  bool costing_p = vectorizable_load_vec_stmt;
  if (!costing_p) {
    stride_step = 0;
    stride_step = cse_and_gimplify_to_preheader(0, stride_step);
  }
  for (; vec_stmts_size;) {
    i = 0;
    for (; i < const_nunits; i++) {
      if (costing_p)
        continue;
      gimple_build_assign(0, POINTER_PLUS_EXPR, vectorizable_load_offvar,
                          stride_step);
    }
    if (const_nunits)
      if (!costing_p) {
        {
          for (; vec_copy_construct_n;)
            new (0) int;
        }
      }
  }
}
