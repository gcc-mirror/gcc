/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
struct S
{
  typedef int i_t;
  static i_t array[42];
};

int S::array[42];

/* Verify that we get two DW_TAG_subrange_type (plus abbrev), and two
   DW_AT_upper_bound, because a different symbolic name is used for
   the array element type.  */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 3 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 2 } } */
