/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { arm*-*-* } { "-march=*" } { "-march=armv7-a" } } */
/* { dg-skip-if "do not override -mcpu" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-a8" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=softfp" } } */
/* { dg-options "-O3 -mcpu=cortex-a8 -mfpu=vfp3 -mfloat-abi=softfp" } */

typedef struct cb
{
  int cxc;
  short int pside;
}  *CBPTR;
typedef struct rwb
{
  int stx;
} RWB;
extern CBPTR *car;
extern RWB *rwAr;
extern int nts;
extern int nRws;
void f()
{
  CBPTR pptr ;
  int  k_lt, k_rt, k_span, rw, p, rt;
  int sa ;
  k_rt = 0;
  k_lt = 10000000;
  for (rw = 1; rw <= nRws; rw++)
    k_lt = rwAr[rw].stx;
  k_span = k_rt - k_lt;
  for (; p <= nts; p++)
    {
      pptr = car[p];
      if (pptr->pside == 3)
        pptr->cxc += (int)(((double)rt / (double) k_span) *((double) sa));
    }
}
