// { dg-do "compile" }
// { dg-options "-O" }
const uint[] cst_ctor_arr;
int test_cst_ctor(const uint[] ptr)
{
    return cst_ctor_arr[2] == 3;
}
// { dg-final { scan-assembler "_d_arraybounds_indexp" } }
