/* Test BTF decl tag generation for structs.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("decl1")))
#define __tag2 __attribute__((btf_decl_tag ("decl2")))
#define __tag3 __attribute__((btf_decl_tag ("decl3")))

struct Foo {
  int a;
  int b __tag3 __tag2;
  char *z __tag1;
};

struct Foo f __tag1 __tag2;

/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT 'Foo'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl3'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT 'Foo'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT 'Foo'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=2" 1} } */

/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 'f'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 'f'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
