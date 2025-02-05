/* Test generation of BTF type tags with typedefs.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_type_tag("1")))
#define __tag2 __attribute__((btf_type_tag("2")))

typedef int *const cp;
typedef int __tag1 *tp;

/* var("x") -> ptr -> type_tag("2") -> typedef("cp") -> const -> ptr -> int  */
cp __tag2 * x;

/* var("y") -> const -> typedef("tp") -> ptr -> type_tag("1") -> int  */
const tp y;

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'x'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '2'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPEDEF 'cp'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPEDEF 'cp'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_CONST" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_CONST ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_INT" 1 } } */

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'y'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_CONST" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_CONST ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPEDEF 'tp'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPEDEF 'tp'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '1'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_INT" 1 } } */
