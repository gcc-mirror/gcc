/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
struct S
{
  S() {}
  ~S() {}
};

const S array[2] = { S(), S() };

/* Like array-3, but with a non-member array without a separate
   declaration, to check that we don't issue the nonsensical
   DW_TAG_const_type used by the member array declaration there.  */
/* { dg-final { scan-assembler-times " DW_TAG_const_type" 4 } } */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
