// { dg-do "compile" }
// { dg-options "-O -fdump-tree-optimized" }
immutable uint[] imm_arr = [1,2,3];
int test_imm(immutable uint[] ptr)
{
    return imm_arr[2] == 3 ? 123 : 456;
}
// { dg-final { scan-assembler-not "_d_arraybounds_indexp" } }
// { dg-final { scan-tree-dump "return 123;" optimized } }
