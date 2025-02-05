/* Test simple generation of BTF type tags.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_type_tag("1")))
#define __tag2 __attribute__((btf_type_tag("2")))

/* var("kp") -> ptr -> type_tag("1") -> int  */
int * __tag1 kp;

struct Foo {
  char a;
  int b;
};

/* var("f") -> ptr -> type_tag("2") -> type_tag("1") -> struct("Foo")  */
struct Foo * __tag1 __tag2 f;

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'kp'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_INT" 1 } } */

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'f'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_PTR" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '1'\\)" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_STRUCT" 1 } } */

/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG" 2 } } */
