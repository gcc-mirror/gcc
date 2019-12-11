/* PR target/92865 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512f -mavx512bw -mxop" } */
/* { dg-final { scan-assembler-times "vpcmp\[bwdq\]\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vpcmpu\[bwdq\]\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]8\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]16\[\t ]" 4 } } *
/* { dg-final { scan-assembler-times "vmovdq\[au\]32\[\t ]" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]64\[\t ]" 4 } } */

extern char arraysb[64];
extern short arraysw[32];
extern int arraysd[16];
extern long long arraysq[8];

extern unsigned char arrayub[64];
extern unsigned short arrayuw[32];
extern unsigned int arrayud[16];
extern unsigned long long arrayuq[8];

int f1(char a)
{
  for (int i = 0; i < 64; i++)
    arraysb[i] = arraysb[i] >= a;
}

int f2(short a)
{
  for (int i = 0; i < 32; i++)
    arraysw[i] = arraysw[i] >= a;
}

int f3(int a)
{
  for (int i = 0; i < 16; i++)
    arraysd[i] = arraysd[i] >= a;
}

int f4(long long a)
{
  for (int i = 0; i < 8; i++)
    arraysq[i] = arraysq[i] >= a;
}

int f5(unsigned char a)
{
  for (int i = 0; i < 64; i++)
    arrayub[i] = arrayub[i] >= a;
}

int f6(unsigned short a)
{
  for (int i = 0; i < 32; i++)
    arrayuw[i] = arrayuw[i] >= a;
}

int f7(unsigned int a)
{
  for (int i = 0; i < 16; i++)
    arrayud[i] = arrayud[i] >= a;
}

int f8(unsigned long long a)
{
  for (int i = 0; i < 8; i++)
    arrayuq[i] = arrayuq[i] >= a;
}
