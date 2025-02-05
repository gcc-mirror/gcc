/* Test BTF type tag generation using C2x standard attribute syntax.
   C2x attribute syntax does not allow attributes to "slide around".  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA -std=c23" } */

#define __tag1 [[gnu::btf_type_tag ("1")]]
#define __tag2 [[gnu::btf_type_tag ("2")]]
#define __tag3 [[gnu::btf_type_tag ("3")]]

/* Note that the BTF format still only allows to represent type tags on
   pointer types, so we do not get any type_tag("1") from the below, as
   it applies to the 'volatile int' type and cannot be represented.  */

/* var(z) -> const -> ptr -> type_tag(2) -> type_tag(3) -> ptr -> type_tag(2) -> volatile -> int  */
volatile int __tag1 * __tag2 * __tag3 __tag2 const z;

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'z'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_CONST" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_CONST ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '2'\\)" 2 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '3'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '3'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_VOLATILE" 1 } } */
