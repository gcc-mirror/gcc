/* { dg-do compile } */
/* { dg-options "-gdwarf-2 -dA" } */
struct S
{
  S() {}
  ~S() {}
  static const S array[2];
};

const S S::array[2] = { S(), S() };

/* Verify that we get only one DW_TAG_subrange_type (plus the abbrev),
   and one DW_AT_upper_bound (non-abbrev), because the array
   definition loses the readonly wrapper for the array type because of
   the dynamic initializers.  The const types are 3: S, S*, and
   S[2], plus the abbrev.  A const version of S[2] doesn't make sense,
   but we output it.  */
/* { dg-final { scan-assembler-times " DW_TAG_const_type" 4 } } */
/* { dg-final { scan-assembler-times " DW_TAG_subrange_type" 2 } } */
/* { dg-final { scan-assembler-times " DW_AT_upper_bound" 1 } } */
