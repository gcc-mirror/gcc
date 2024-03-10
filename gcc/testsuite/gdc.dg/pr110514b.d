// { dg-do "compile" }
// { dg-options "-O" }
immutable uint[] imm_ctor_arr;
int test_imm_ctor(immutable uint[] ptr)
{
    return imm_ctor_arr[2] == 3;
}
// { dg-final { scan-assembler "_d_arraybounds_indexp" } }
