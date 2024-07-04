/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O2 -ftree-vectorize -flto -fno-use-linker-plugin -flto-partition=none -mrvv-max-lmul=dynamic" } */

void (*foo[6][6]) (int);
void bar (hdR)
    int hdR;
{ }
void xxx ()
{
    unsigned int i, j;
    for (i = 0; i < 6; ++i)
	for (j = 0; j < 6; ++j)
            foo [i][j] = bar;
}
