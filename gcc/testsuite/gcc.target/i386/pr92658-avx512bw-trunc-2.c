/* PR target/92658 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpmovwb" 1 } } */
/* { dg-final { scan-assembler-times "vpmovdb" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpmovdw" 1 { target { ! ia32 } } } } */

void
foo (int* __restrict a, char* b)
{
    b[0] = a[0];
    b[1] = a[1];
}

void
foo2 (short* __restrict a, char* b)
{
    b[0] = a[0];
    b[1] = a[1];
}

void
foo3 (int* __restrict a, short* b)
{
    b[0] = a[0];
    b[1] = a[1];
}
