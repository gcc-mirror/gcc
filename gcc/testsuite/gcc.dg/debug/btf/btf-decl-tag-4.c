/* Test BTF decl tag generation with BTF pruning.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -gprune-btf -dA" } */

#define __decl1 __attribute__((btf_decl_tag ("decl1")))
#define __decl2 __attribute__((btf_decl_tag ("decl2")))
#define __decl3 __attribute__((btf_decl_tag ("decl3")))

struct S {
  /* This tag on S.v shall not be emitted, because struct S is pruned and
     replaced with a FWD, which does not hold any member info.  */
  int v __decl3;
  int w;
};

struct T {
  int a;
  struct S *s __decl1;
  int c __decl2;
};

struct T t __decl1;

int __decl1 func (struct T *t __decl2)
{
  return t->a + t->c;
}

/* { dg-final { scan-assembler-not " BTF_KIND_DECL_TAG 'decl3'" } } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT 'T'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT 'T'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=2" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 't'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=0" 1} } */
