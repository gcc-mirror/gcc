/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

#include <altivec.h>
#include <stdio.h>

void abort (void);

int main() {
  int i;
  vector float vfa, vfb;
  vector unsigned short vresult, vexpected;

  vfa = (vector float){0.4, 1.6, 20.0, 99.9 };
  vfb = (vector float){10.0, -2.0, 70.0, 999.0 };

  /* Expected results.  */
  vexpected = (vector unsigned short) { 0x3666, 0x3e66, 0x4d00, 0x563e,
					0x4900, 0xc000, 0x5460, 0x63ce};

/*
     vresult = vec_pack_to_short_fp32 (vfa, vfb);
  This built-in converts a pair of vector floats into a single vector of
  packed half-precision (F16) values.  The result type is a vector of
  signed shorts.
  The expected codegen for this builtin is
    xvcvsphp t, vfa
    xvcvsphp u, vfb
    if (little endian)
      vpkuwum vresult, t, u
    else
      vpkuwum vresult, u, t
*/

  vresult = vec_pack_to_short_fp32 (vfa, vfb);

#ifdef DEBUG
  for(i = 0; i< 4; i++) { printf("i=[%d] %f  \n",i,vfa[i]); }
  for(i = 0; i< 4; i++) { printf("i=[%d] %f  \n",i+4,vfb[i]); }
  for(i = 0; i< 8; i++) { printf("i=[%d] %d  \n",i,vresult[i]); }
#endif

  for(i = 0; i< 8; i++) {
    if (vresult[i] != vexpected[i]) {
	printf("i=[%d] 0x%x != 0x%x \n",i,vresult[i],vexpected[i]);
      abort();
    }
  }
}
