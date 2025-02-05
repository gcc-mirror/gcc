/* Test generation of BTF type tags with cv-quals.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag __attribute__((btf_type_tag("1")))

/* var("pci") -> const -> ptr -> type_tag("1") -> int  */
int __tag *const pci;

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'pci'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_CONST" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_CONST ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_INT" 1 } } */
