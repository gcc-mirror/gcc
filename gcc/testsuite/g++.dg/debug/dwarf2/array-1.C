/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
struct S
{
  static int array[];
};

int S::array[42];

/* Verify that we get two DW_TAG_subrange_type, only one of which with
   a DW_AT_upper_bound.  */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 4 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
