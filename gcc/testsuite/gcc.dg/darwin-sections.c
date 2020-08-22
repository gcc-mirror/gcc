/* { dg-do compile {target *-*-darwin* } } */
/* { dg-options "-std=c99 -w -fcommon" } */

/* This has been designed to give the same section usage for
   -m32 and -m64 - so don't put any ints or longs in it ... */

/* A zero-sized object.  */
typedef struct _empty {} e_s;

/* These should go in .comm */
char ub;
e_s ea;
/* { dg-final { scan-assembler ".comm\[\t \]_ub,1" } } */
/* { dg-final { scan-assembler ".comm\[\t \]_ea,1" } } */

/* These should go into __DATA,__common */
char a = 0;
short b = 0;
long long d = 0;
float e = 0;
double f = 0;
long double g = 0.L;
long long al_256 __attribute__((aligned (256))) = 0;
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_a,1,0} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_b,2,1} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_d,8,3} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_e,4,2} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_f,8,3} } } */
/* long double can be 64 or 128 bits depending on the Darwin subtarget.  */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_g,(16,4|8,3)} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__common,_al_256,8,8} } } */

/* These should go into __DATA,__bss */
static e_s sea;
static char sa ;
static short sb ;
static long long sd;
static float se ;
static double sf ;
static long double sg;
static long long sal_256 __attribute__((aligned (2048)));
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sea,1,0} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sa,1,0} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sb,2,1} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sd,8,3} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_se,4,2} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sf,8,3} } } */
/* long double can be 64 or 128 bits depending on the Darwin subtarget.  */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sg,(16,4|8,3)} } } */
/* { dg-final { scan-assembler {.zerofill __DATA,__bss,_sal_256,8,11} } } */

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
