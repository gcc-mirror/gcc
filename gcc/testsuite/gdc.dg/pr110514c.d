// { dg-do "compile" }
// { dg-options "-O" }
const uint[] cst_arr = [1,2,3];
int test_cst(const uint[] ptr)
{
    return cst_arr[2] == 3;
}
// { dg-final { scan-assembler "_d_arraybounds_indexp" } }
