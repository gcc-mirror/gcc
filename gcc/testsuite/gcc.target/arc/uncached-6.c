/* { dg-options "-O1" } */
/* { dg-do compile } */

#define RegRdSI(v,a) ((v) = *(volatile __attribute__((uncached)) int *)(a))
#define RegRdQI(v,a) ((v) = *(volatile __attribute__((uncached)) char *)(a))
#define RegRdHI(v,a) ((v) = *(volatile __attribute__((uncached)) short *)(a))
#define RegRdDI(v,a) \
  ((v) = *(volatile __attribute__((uncached)) long long *)(a))

char a0;
short a1;
int a2;
long long a3;

void foox (void *p)
{
  RegRdQI (a0, p++);
  RegRdHI (a1, p++);
  RegRdSI (a2, p++);
  RegRdDI (a3, p  );
}

void barx (int arg)
{
  RegRdQI (a0, 0x40000);
  RegRdHI (a1, 0x40010);
  RegRdSI (a2, 0x40020);
  RegRdDI (a3, 0x40040);
}

/* { dg-final { scan-assembler-times "ldb\.di" 2 } } */
/* { dg-final { scan-assembler-times "ld\[hw\]\.di" 2 } } */
/* { dg-final { scan-assembler-times "ldd\.di" 2 { target { ll64 } } } } */
/* { dg-final { scan-assembler-times "ld\.di" 2 { target { ll64 } } } } */
/* { dg-final { scan-assembler-times "ld\.di" 6 { target { ! { ll64 } } } } } */
