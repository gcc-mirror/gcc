/* { dg-do compile {target *-*-darwin* } } */
/* { dg-options "-std=c99 -w" } */

/* This has been designed to give the same section usage for
   -m32 and -m64 - so don't put any ints or longs in it ... */

/* A zero-sized object.  */
typedef struct _empty {} e_s;

/* These should go in .comm */
char ub;
e_s ea;
/* { dg-final { scan-assembler ".comm\[\t \]_ub,1" } } */
/* { dg-final { scan-assembler ".comm\[\t \]_ea,1" } } */

/* These should go into .data */
char a = 0;
short b = 0;
/* { dg-final { scan-assembler ".globl _a.*.data.*.space\[\t \]1" } } */
/* { dg-final { scan-assembler ".globl _b.*.data.*.space\[\t \]2" } } */

/* These should go into __pu_bssN */
long long d = 0;
float e = 0;
double f = 0;
long double g = 0.L;
long long al_256 __attribute__((aligned (256))) = 0;
/* { dg-final { scan-assembler ".zerofill __DATA,__pu_bss3,_d,8,3" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__pu_bss2,_e,4,2" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__pu_bss3,_f,8,3" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__pu_bss4,_g,16,4" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__pu_bss8,_al_256,8,8" } } */

/* This should go into __zo_bss0 */
static e_s sea;
/* { dg-final { scan-assembler ".zerofill __DATA,__zo_bss0,_sea,1" } } */

/* These should go into .static_data */
static char sa ;
static short sb ;
/* { dg-final { scan-assembler ".static_data.*_sa:.*.space\[\t \]1" } } */
/* { dg-final { scan-assembler ".static_data.*_sb:.*.space\[\t \]2" } } */

/* These should go into _bssN */
static long long sd;
static float se ;
static double sf ;
static long double sg;
static long long sal_256 __attribute__((aligned (2048)));
/* { dg-final { scan-assembler ".zerofill __DATA,__bss3,_sd,8,3" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__bss2,_se,4,2" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__bss3,_sf,8,3" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__bss4,_sg,16,4" } } */
/* { dg-final { scan-assembler ".zerofill __DATA,__bss11,_sal_256,8,11" } } */

long long foo (int x)
{
  e_s *s;
  a += x + sa;
  b += a + sb;
  d += b + sd;
  e += d + se;
  f += e + sf;
  g += f + sg;
 
  s = &ea;
  s = &sea;
  
  b += al_256;
  b += sal_256;

  return (long long) sd + b;
}
