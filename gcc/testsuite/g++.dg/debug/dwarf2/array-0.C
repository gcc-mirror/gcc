/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
struct S
{
  static int array[42];
};

int S::array[42];

/* Verify that we get only one DW_TAG_subrange_type with a
   DW_AT_upper_bound.  */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
