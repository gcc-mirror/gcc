/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-march=skylake-avx512" { target x86_64-*-* i?86-*-* } } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

extern unsigned long long int arr_86[];
extern unsigned long long int arr_87[][15];

void test(_Bool a, unsigned short c[][15], unsigned char d[])
{
  for (short h = 0; h < 10; h++)
    for (char i = 0; i < 15; i += 2)
      {
	arr_86[0] = d[0];
	arr_87[h][0] = a ? c[h][i] : 0;
      }
}
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
