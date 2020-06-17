/* { dg-do compile } */
/* { dg-additional-options "-march=skylake" { target x86_64-*-* i?86-*-* } } */

typedef struct {
  unsigned num;
} VEC_tree_base;

enum {
  LTO_DECL_STREAM_NAMESPACE_DECL,
  LTO_DECL_STREAM_LABEL_DECL,
  LTO_N_DECL_STREAMS
};

struct lto_tree_ref_encoder {
  VEC_tree_base *trees;
} typedef *lto_out_decl_state_ptr;

typedef struct {
  lto_out_decl_state_ptr vec[1];
} VEC_lto_out_decl_state_ptr_base;

VEC_lto_out_decl_state_ptr_base *a;
int f;
long g;
int
fn1(struct lto_tree_ref_encoder *p1) {
  int i;
  long b;
  i = 0;
  for (; i < LTO_N_DECL_STREAMS; i++) {
    struct lto_tree_ref_encoder c = *p1;
    int d;
    VEC_tree_base *e = c.trees;
    d = e ? e->num : 0;
    b += d;
  }
  return b;
}
int
fn2() {
  lto_out_decl_state_ptr h;
  struct lto_tree_ref_encoder j;
  unsigned k;
  for (; k < f; k++) {
    h = a->vec[k];
    j = *h;
    g += fn1(&j);
  }
}
