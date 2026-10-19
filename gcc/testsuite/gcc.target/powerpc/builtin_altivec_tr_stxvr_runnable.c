/* Test of __builtin_vec_xst_trunc  */

/* { dg-do run { target power10_hw } } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

#include <altivec.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <stdlib.h>

#define DEBUG 0

vector signed __int128 store_data =
  {  (__int128) 0x8ACE000000000000 << 64 | (__int128) 0xfedcba9876543217ULL};

union conv_t {
  vector signed __int128 vsi128;
  unsigned long long ull[2];
} conv;

void abort (void);


int
main () {
  int i;
  signed long sl;
  signed char sc[2], expected_sc;
  signed short ss[2], expected_ss;
  signed int si[2], expected_si;
  signed long long int sll[2], expected_sll;
  signed char *psc;
  signed short *pss;
  signed int *psi;
  signed long long int *psll;
  
#if DEBUG
  conv.vsi128 = store_data;
  printf("Data to store 0x%llx %llx\n", conv.ull[1], conv.ull[0]);
#endif

  psc = &sc[0];
  pss = &ss[0];
  psi = &si[0];
  psll = &sll[0];

  sl = 1;
  sc[0] = 0xA1;
  expected_sc = 0xA1;
  __builtin_altivec_tr_stxvrbx (store_data, sl, psc);

  if (expected_sc != sc[0] & 0xFF)
#if DEBUG
    printf(" ERROR: Signed char = 0x%x doesn't match expected value 0x%x\n",
	   sc[0] & 0xFF, expected_sc);
#else
    abort();
#endif

  ss[0] = 0x52;
#ifdef __BIG_ENDIAN__
  expected_ss = 0x0032;
#else
  expected_ss = 0x1752;
#endif
  __builtin_altivec_tr_stxvrhx (store_data, sl, pss);

  if (expected_ss != ss[0] & 0xFFFF)
#if DEBUG
    printf(" ERROR: Signed short = 0x%x doesn't match expected value 0x%x\n",
	   ss[0], expected_ss) & 0xFFFF;
#else
    abort();
#endif

  si[0] = 0x21;
#ifdef __BIG_ENDIAN__
  expected_si = 0x00765432;
#else
  expected_si = 0x54321721;
#endif
  __builtin_altivec_tr_stxvrwx (store_data, sl, psi);

  if (expected_si != si[0])
#if DEBUG
    printf(" ERROR: Signed int = 0x%x doesn't match expected value 0x%x\n",
	   si[0], expected_si);
#else
    abort();
#endif

  sll[0] = 0x12FFULL;
#ifdef __BIG_ENDIAN__
  expected_sll = 0x00fedcba98765432;
#else
  expected_sll = 0xdcba9876543217FF;
#endif
  __builtin_altivec_tr_stxvrdx (store_data, sl, psll);

  if (expected_sll != sll[0])
#if DEBUG
    printf(" ERROR: Signed long long int = 0x%llx doesn't match expected value 0x%llx\n",
	   sll[0], expected_sll);
#else
    abort();
#endif

  return 0;
}

/* { dg-final { scan-assembler-times {\mstxvrbx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvrhx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvrwx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mstxvrdx\M} 1 } } */
