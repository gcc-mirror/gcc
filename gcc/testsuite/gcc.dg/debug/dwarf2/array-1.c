/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
extern int array[];

int array[42];

/* Verify that we get two DW_TAG_subtrange_type (each with an abbrev),
   but only one DW_AT_upper_bound.  */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 4 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
