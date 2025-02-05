/* Test BTF decl tag generation for functions and function args.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("decl1")))
#define __tag2 __attribute__((btf_decl_tag ("decl2")))
#define __tag3 __attribute__((btf_decl_tag ("decl3")))

int __tag1 __tag2 func (int arg_a __tag3 __tag1, int arg_b __tag2)
{
  return arg_a * arg_b;
}

int foo (int x) {
  return func (x, x + 1);
}

/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=0" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl3'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=0" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_FUNC 'func'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=1" 1} } */
