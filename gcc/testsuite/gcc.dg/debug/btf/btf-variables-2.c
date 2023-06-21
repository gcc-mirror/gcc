/* BTF generation for variables with removed type.

   BTF does not support vector types, so no representation for the type
   of 'bar' will be emitted. In this test, we check to also ensure that the
   variable 'bar' is not emitted, as it references a type that is not supported
   in BTF.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* We expect only 3 variables.  */
/* { dg-final { scan-assembler-times "\[\t \]0xe000000\[\t \]+\[^\n\]*btv_info" 3 } } */

/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'foo'" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'baz'" 1 } } */
/* { dg-final { scan-assembler-times " BTF_KIND_VAR 'myst'" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"foo.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"baz.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"myst.0\"\[\t \]+\[^\n\]*btf_string" 1 } } */

int foo;
float __attribute__((__vector_size__(16))) bar;
int baz[10];

struct st
{
  int a;
  int b : 6;
  int c : 2;
} myst;
