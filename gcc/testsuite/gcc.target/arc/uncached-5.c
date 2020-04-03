/* { dg-options "-O1" } */
/* { dg-do compile } */

#define RegWrSI(a,v)  (*(volatile __attribute__((uncached)) int *)(a)=(v))
#define RegWrQI(a,v)  (*(volatile __attribute__((uncached)) char *)(a)=(v))
#define RegWrHI(a,v)  (*(volatile __attribute__((uncached)) short *)(a)=(v))
#define RegWrDI(a,v)  (*(volatile __attribute__((uncached)) long long *)(a)=(v))

void foo (int arg, void *p)
{
  RegWrDI (p  , arg);
  RegWrHI (p++, arg);
  RegWrSI (p++, arg);
  RegWrQI (p++, arg);
}

void bar (void)
{
  RegWrQI (0x40000, 1);
  RegWrHI (0x40010, 2);
  RegWrSI (0x40020, 4);
  RegWrDI (0x40040, 8);
}

/* { dg-final { scan-assembler-times "stb\.di" 2 } } */
/* { dg-final { scan-assembler-times "st\[hw\]\.di" 2 } } */
/* { dg-final { scan-assembler-times "std\.di" 2 { target { ll64 } } } } */
/* { dg-final { scan-assembler-times "st\.di" 2 { target { ll64 } } } } */
/* { dg-final { scan-assembler-times "st\.di" 6 { target { ! { ll64 } } } } } */
