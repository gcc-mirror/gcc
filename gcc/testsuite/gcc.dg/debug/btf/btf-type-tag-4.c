/* Test generation of BTF type tag when applied to function prototypes.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

#define __tag1 __attribute__((btf_type_tag("1")))
#define __tag2 __attribute__((btf_type_tag("2")))

int * __tag1
dothing (void __tag2 *ptr, int __tag2 __tag1 *xi)
{
  if (xi)
    {
      int *tmp = (int *) ptr;
      return tmp + (*xi);
    }

  return (int *) ptr;
}

/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2} btt_type: void" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '2'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_INT" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_TYPE_TAG '1'(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '2'" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '1'" 2 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_PTR ''(\[\\r\\n\]+\[^\\r\\n\]*){2}\\(BTF_KIND_TYPE_TAG '2'" 1 } } */
