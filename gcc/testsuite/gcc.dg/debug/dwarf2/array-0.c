/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
extern int array[42];

int array[42];

/* Verify that we get only one DW_TAG_subtrange_type (plus abbrev),
   with a DW_AT_upper_bound.  */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
