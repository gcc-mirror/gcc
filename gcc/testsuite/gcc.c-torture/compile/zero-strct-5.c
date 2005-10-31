/* Check that the inliner does not crash for this testcase.
   gimple_expr can change the expr to NULL meaning that we
   should not add any statement. */
struct f {};
struct g1 {struct f l;};

static inline void g(struct f a, int i){}

void h(void)
{
  struct g1 t;
  g(t.l , 1);
}

