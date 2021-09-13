/* PR target/100093  */
/* { dg-do compile } */
/* { dg-options "-O3 -march=znver1" } */
/* { dg-final { scan-assembler-not "vextractf128" } } */

__attribute__((target("tune=skylake-avx512")))
void fill_avx2(double *__restrict__ data, int n, double value)
{
    for (int i = 0; i < n * 16; i++) {
        data[i] = value;
    }
}
