/* { dg-do run { target powerpc*-*-* } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O0 -Wall" } */

#include <altivec.h>
#include <stdlib.h>

/* vec_ldl and vec_lvxl (an alias for vec_ldl) do an aligned vector
 * load from memory, marking the fetched memory as least recently used
 * (hinting that we do not anticipate a need to fetch this vector
 * again within the near future.)
 *
 * vector <TYPE> result = vec_ldl (int offset, <TYPE> *address)
 * vector <TYPE> result = vec_ldl (int offset, vector <TYPE> *address)
 *
 * The effect of these instructions is to perform the following:
 *
 *  resuilt = *((vector <TTYPE> *)((((char *) address) + offset) & ~0x0f))
 *
 * This test exercises the following new prototypes of the vec_ldl
 * service which were added in late March 2018:
 *
 *  vector bool int vec_ldl (int, bool int *)
 *  vector bool short vec_ldl (int, bool short *)
 *  vector bool char vec_ldl (int, bool char *)
 *  vector double vec_ldl (int, double *)
 *  vector long long int vec_ldl (int, long long int *)
 *  vector unsigned long long int vec_ldl (int, unsigned long long int *)
 *  vector bool long long vec_ldl (int, bool long long *)
 */

static signed char ca[64] __attribute__((aligned(16)));
static unsigned char uca[64] __attribute__((aligned(16)));

static vector signed char *vcp = (vector signed char *) ca;
static unsigned vector char *vucp = (vector unsigned char *) uca;

static short sa[32] __attribute__((aligned(16)));
static unsigned short usa[32] __attribute__((aligned(16)));

static vector short *vsp = (vector short *) sa;
static unsigned vector short *vusp = (vector unsigned short *) usa;

static int ia[16] __attribute__((aligned(16)));
static unsigned int uia[16] __attribute__((aligned(16)));

static vector int *vip = (vector int *) ia;
static unsigned vector int *vuip = (vector unsigned int *) uia;

static long long la[8] __attribute__((aligned(16)));
static unsigned long long ula[8] __attribute__((aligned(16)));

static vector long long *vlp = (vector long long *) la;
static unsigned vector long long *vulp = (vector unsigned long long *) ula;

static double da[8] __attribute__((aligned(16)));
static vector double *vdp = (vector double *) da;


void
doInitialization ()
{
  unsigned int i;

  for (i = 0; i < 64; i++)
    ca[i] = uca[i] = i;

  for (i = 0; i < 32; i++)
    sa[i] = usa[i] = i;

  for (i = 0; i < 16; i++)
    ia[i] = uia[i] = i;

  for (i = 0; i < 8; i++)
    la[i] = ula[i] = i;

  for (i = 0; i < 8; i++)
    da[i] = 0.125 * i;
}

int
main (int argc, char *argv[])
{
  vector long long int lv;
  vector unsigned long long int ulv;
  vector int iv;
  vector unsigned int uiv;
  vector short sv;
  vector unsigned short usv;
  vector signed char cv;
  vector unsigned char ucv;
  vector double dv;

  doInitialization ();

  /* Do vector of char.  */
  for (int i = 0; i < 16; i++) {
    /* Focus on ca[16] ... ca[31].  */
    cv = vec_ldl (i+16, ca);	/* compiler: invalid parameter combination */
    if (cv[4] != ca[20])
      abort ();
    /* Focus on uca[32] ... uca[47].  */
    ucv = vec_ldl (i+32, uca);
    if (ucv[7] != uca[39])
      abort ();
    /* Focus on ca[0] ... ca[15].  */
    cv = vec_ldl (i, vcp);
    if (cv[3] != ca[3])
      abort ();
    /* Focus on ca[0] ... ca[15] while i <= 8.
       Focus on ca[16] ... ca[31] while i > 8.  */
    ucv = vec_ldl (i+7, vucp);
    if ((i+7 > 15) && (ucv[13] != uca[29]))
      abort ();
    if ((i + 7 <= 15) && (ucv[13] != uca[13]))
      abort ();
  }

  /* Do vector of short.  */
  for (int i = 0; i < 16; i++) {
    /* Focus on sa[8] ... sa[15].  */
    sv = vec_ldl (i+16, sa);
    if (sv[4] != sa[12])
      abort ();
    /* Focus on usa[24] ... usa[31].  */
    usv = vec_ldl (i+48, usa);
    if (usv[7] != usa[31])
      abort ();
    /* Focus on sa[0] ... sa[7].  */
    sv = vec_ldl (i, vsp);
    if (sv[3] != sa[3])
      abort ();
    /* Focus on usa[0] ... usa[7] while i <= 8.
       Focus on usa[8] ... usa[15] while i > 8.  */
    usv = vec_ldl (i+7, vusp);
    if ((i+7 > 15) && (usv[5] != usa[13]))
      abort ();
    if ((i + 7 <= 15) && (usv[5] != usa[5]))
      abort ();
  }

  /* Do vector of int.  */
  for (int i = 0; i < 16; i++) {
    /* Focus on ia[8] ... ia[11].  */
    iv = vec_ldl (i+32, ia);
    if (iv[3] != ia[11])
      abort ();
    /* Focus on uia[12] ... uia[15].  */
    uiv = vec_ldl (i+48, uia);
    if (uiv[2] != uia[14])
      abort ();
    /* Focus on ia[0] ... ia[3].  */
    iv = vec_ldl (i, vip);
    if (iv[3] != ia[3])
      abort ();
    /* Focus on uia[0] ... uia[3] while i <= 8.
       Focus on uia[4] ... uia[7] while i > 8.  */
    uiv = vec_ldl (i+7, vuip);
    if ((i+7 > 15) && (uiv[1] != uia[5]))
      abort ();
    if ((i + 7 <= 15) && (uiv[1] != uia[1]))
      abort ();
  }

  /* Do vector of long long int.  */
  for (int i = 0; i < 16; i++) {
    /* Focus on la[4] ... la[5].  */
    lv = vec_ldl (i+32, la);
    if (lv[1] != la[5])
      abort ();
    /* Focus on ula[6] ... ula[7].  */
    ulv = vec_ldl (i+48, ula);
    if (ulv[0] != uia[6])
      abort ();
    /* Focus on la[0] ... la[1].  */
    lv = vec_ldl (i, vlp);
    if (iv[1] != la[1])
      abort ();
    /* Focus on ula[0] ... uia[1] while i <= 8.
       Focus on uia[2] ... uia[3] while i > 8.  */
    ulv = vec_ldl (i+7, vulp);
    if ((i+7 > 15) && (ulv[1] != ula[3]))
      abort ();
    if ((i + 7 <= 15) && (ulv[1] != ula[1]))
      abort ();
  }

  /* Do vector of double.  */
  for (int i = 0; i < 16; i++) {
    /* Focus on da[2] ... da[3].  */
    dv = vec_ldl (i+16, da);
    if (dv[1] != da[3])
      abort ();
    /* Focus on da[6] ... da[7].  */
    dv = vec_ldl (i+48, vdp);
    if (dv[0] != da[6])
      abort ();
    /* Focus on da[0] ... da[1].  */
    dv = vec_ldl (i, da);
    if (dv[1] != da[1])
      abort ();
    /* Focus on da[0] ... da[1] while i <= 8.
       Focus on da[2] ... da[3] while i > 8.  */
    dv = vec_ldl (i+7, vdp);
    if ((i+7 <= 15) && (dv[1] != da[1]))
      abort ();
    if ((i + 7 > 15) && (dv[1] != da[3]))
      abort ();
  }
  return 0;
}
