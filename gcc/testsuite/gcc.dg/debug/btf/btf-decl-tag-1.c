/* Test simple BTF decl tag generation for variables.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_decl_tag ("decl1")))
#define __tag2 __attribute__((btf_decl_tag ("decl2")))
#define __tag3 __attribute__((btf_decl_tag ("decl3")))

int x __tag1 __tag2;
int y __tag1;

/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 'x'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 'x'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
/* { dg-final { scan-assembler-times " BTF_KIND_DECL_TAG 'decl1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VAR 'y'\\)\[\\r\\n\]+\[^\\r\\n\]*component_idx=-1" 1} } */
