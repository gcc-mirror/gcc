/* Test BTF generation for struct type with a member which refers to an
   unsupported type.

   BTF does not support vector types (among other things). When
   generating BTF for a struct (or union) type.  Members which refer to
   unsupported types should not be skipped, however.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* Expect a struct with 3 members - 'f' is present but is of data type void.  */
/* { dg-final { scan-assembler-times "\[\t \]0x4000003\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times " MEMBER 'f' idx=1\[\\r\\n\]+\[^\\r\\n\]*0\[\t \]+\[^\n\]*btm_type: void" 1 } } */

struct with_float
{
  int a;
  float __attribute__((__vector_size__(16))) f;
  char c;
} instance;
