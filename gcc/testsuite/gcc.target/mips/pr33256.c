/* GCC used to report an ICE for this test because we generated a LO_SUM
   for an illegitimate constant.  */
/* { dg-mips-options "-mabi=64 -mips3 -msym32 -O2 -EB -mno-abicalls" } */
extern unsigned long a[];
int b (int);

int
c (void)
{
  return b (a[0]);
}
