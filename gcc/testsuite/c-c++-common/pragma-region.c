/* { dg-options "-Wunknown-pragmas" } */
/* { dg-final { scan-assembler "code_within_region" } } */
/* { dg-final { scan-assembler "code_within_named_region" } } */

#pragma region
void code_within_region() { }
#pragma endregion

#pragma region ignored name
void code_within_named_region() { }
#pragma endregion // ignored comment
