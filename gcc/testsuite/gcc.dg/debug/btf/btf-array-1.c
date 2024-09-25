/* BTF generation for array type.

   Unsized arrays are encoded with a 0 for the number of elements.

   In this testcase, 5 distinct BTF records for arrays are expected
   b1 : cta_nelems = 2
   c1 : cta_nelems = 3
   a1 : cta_nelems = 2, 5
   buf : cta_nelems = 0.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "0x3000000\[\t \]+\[^\n\]*btt_info" 5 } } */

/* { dg-final { scan-assembler-times "\[\t \]0x2\[\t \]+\[^\n\]*bta_nelems" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x3\[\t \]+\[^\n\]*bta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0x5\[\t \]+\[^\n\]*bta_nelems" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*bta_nelems" 1 } } */

/* { dg-final { scan-assembler-times " bta_elem_type: \\(BTF_KIND_INT 'int'\\)" 4 } } */
/* { dg-final { scan-assembler-times " bta_elem_type: \\(BTF_KIND_ARRAY ''\\)" 1 } } */

int b1[2] = {0,1};
int c1[5] = {0,1,2,3,4};
int a1[2][3] = { {3,4,5}, {2,3,4} };

/* Variable length struct using arrays.  */
struct my_array
{
  int flags;
  int length;
  int buf[];
} my_array_obj;
