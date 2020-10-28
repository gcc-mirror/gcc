/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */
#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

extern void abort (void);

int
main (int argc, char *argv [])
{
  int i;
  unsigned int index;
  vector unsigned char vresult_ch;
  vector unsigned char expected_vresult_ch;
  vector unsigned char src_va_ch;
  vector unsigned char src_vb_ch;
  unsigned char src_a_ch;

  vector unsigned short vresult_sh;
  vector unsigned short expected_vresult_sh;
  vector unsigned short src_va_sh;
  vector unsigned short src_vb_sh;
  unsigned short int src_a_sh;

  vector unsigned int vresult_int;
  vector unsigned int expected_vresult_int;
  vector unsigned int src_va_int;
  vector unsigned int src_vb_int;
  unsigned int src_a_int;
  
  vector unsigned long long vresult_ll;
  vector unsigned long long expected_vresult_ll;
  vector unsigned long long src_va_ll;
  unsigned long long int src_a_ll;

  /* Vector insert, low index, from GPR */
  src_a_ch = 79;
  index = 2;
  src_va_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
  vresult_ch = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ch = (vector unsigned char) { 0, 1, 79, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
						 
    vresult_ch = vec_insertl (src_a_ch, src_va_ch, index);

  if (!vec_all_eq (vresult_ch,  expected_vresult_ch)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_a_ch, src_va_ch, index)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_ch[%d] = %d, expected_vresult_ch[%d] = %d\n",
	     i, vresult_ch[i], i, expected_vresult_ch[i]);
#else
    abort();
#endif
  }

  src_a_sh = 79;
  index = 10;
  src_va_sh = (vector unsigned short int) { 0, 1, 2, 3, 4, 5, 6, 7 };
  vresult_sh = (vector unsigned short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector unsigned short int) { 0, 1, 2, 3,
						      4, 79, 6, 7 };

  vresult_sh = vec_insertl (src_a_sh, src_va_sh, index);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_a_sh, src_va_sh, index)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  src_a_int = 79;
  index = 8;
  src_va_int = (vector unsigned int) { 0, 1, 2, 3 };
  vresult_int = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector unsigned int) { 0, 1, 79, 3 };

  vresult_int = vec_insertl (src_a_int, src_va_int, index);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_a_int, src_va_int, index)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  src_a_ll = 79;
  index = 8;
  src_va_ll = (vector unsigned long long) { 0, 1 };
  vresult_ll = (vector unsigned long long) { 0, 0 };
  expected_vresult_ll = (vector unsigned long long) { 0, 79 };

  vresult_ll = vec_insertl (src_a_ll, src_va_ll, index);

  if (!vec_all_eq (vresult_ll,  expected_vresult_ll)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_a_ll, src_va_ll, index)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ll[%d] = %d, expected_vresult_ll[%d] = %d\n",
	     i, vresult_ll[i], i, expected_vresult_ll[i]);
#else
    abort();
#endif
  }

  /* Vector insert, low index, from vector */
  index = 2;
  src_va_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
  src_vb_ch = (vector unsigned char) { 10, 11, 12, 13, 14, 15, 16, 17,
				       18, 19, 20, 21, 22, 23, 24, 25 };
  vresult_ch = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ch = (vector unsigned char) { 0, 1, 18, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
						 
  vresult_ch = vec_insertl (src_vb_ch, src_va_ch, index);

  if (!vec_all_eq (vresult_ch,  expected_vresult_ch)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_vb_ch, src_va_ch, index)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_ch[%d] = %d, expected_vresult_ch[%d] = %d\n",
	     i, vresult_ch[i], i, expected_vresult_ch[i]);
#else
    abort();
#endif
  }

  index = 4;
  src_va_sh = (vector unsigned short) { 0, 1, 2, 3, 4, 5, 6, 7 };
  src_vb_sh = (vector unsigned short) { 10, 11, 12, 13, 14, 15, 16, 17 };
  vresult_sh = (vector unsigned short) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector unsigned short) { 0, 1, 14, 3, 4, 5, 6, 7 };
						 
  vresult_sh = vec_insertl (src_vb_sh, src_va_sh, index);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_vb_sh, src_va_sh, index)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  index = 8;
  src_va_int = (vector unsigned int) { 0, 1, 2, 3 };
  src_vb_int = (vector unsigned int) { 10, 11, 12, 13 };
  vresult_int = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector unsigned int) { 0, 1, 12, 3 };
						 
  vresult_int = vec_insertl (src_vb_int, src_va_int, index);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_insertl (src_vb_int, src_va_int, index)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  /* Vector insert, high index, from GPR */
  src_a_ch = 79;
  index = 2;
  src_va_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
  vresult_ch = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 79, 14, 15 };
						 
    vresult_ch = vec_inserth (src_a_ch, src_va_ch, index);

  if (!vec_all_eq (vresult_ch,  expected_vresult_ch)) {
#if DEBUG
   printf("ERROR, vec_inserth (src_a_ch, src_va_ch, index)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_ch[%d] = %d, expected_vresult_ch[%d] = %d\n",
	     i, vresult_ch[i], i, expected_vresult_ch[i]);
#else
    abort();
#endif
  }

  src_a_sh = 79;
  index = 10;
  src_va_sh = (vector unsigned short int) { 0, 1, 2, 3, 4, 5, 6, 7 };
  vresult_sh = (vector unsigned short int) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector unsigned short int) { 0, 1, 79, 3,
						      4, 5, 6, 7 };

  vresult_sh = vec_inserth (src_a_sh, src_va_sh, index);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_a_sh, src_va_sh, index)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  src_a_int = 79;
  index = 8;
  src_va_int = (vector unsigned int) { 0, 1, 2, 3 };
  vresult_int = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector unsigned int) { 0, 79, 2, 3 };

  vresult_int = vec_inserth (src_a_int, src_va_int, index);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_a_int, src_va_int, index)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  src_a_ll = 79;
  index = 8;
  src_va_ll = (vector unsigned long long) { 0, 1 };
  vresult_ll = (vector unsigned long long) { 0, 0 };
  expected_vresult_ll = (vector unsigned long long) { 79, 1 };

  vresult_ll = vec_inserth (src_a_ll, src_va_ll, index);

  if (!vec_all_eq (vresult_ll,  expected_vresult_ll)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_a_ll, src_va_ll, index)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_ll[%d] = %d, expected_vresult_ll[%d] = %d\n",
	     i, vresult_ll[i], i, expected_vresult_ll[i]);
#else
    abort();
#endif
  }

  /* Vector insert, left index, from vector */
  index = 2;
  src_va_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 13, 14, 15 };
  src_vb_ch = (vector unsigned char) { 10, 11, 12, 13, 14, 15, 16, 17,
				       18, 19, 20, 21, 22, 23, 24, 25 };
  vresult_ch = (vector unsigned char) { 0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_ch = (vector unsigned char) { 0, 1, 2, 3, 4, 5, 6, 7,
				       8, 9, 10, 11, 12, 18, 14, 15 };
						 
  vresult_ch = vec_inserth (src_vb_ch, src_va_ch, index);

  if (!vec_all_eq (vresult_ch,  expected_vresult_ch)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_vb_ch, src_va_ch, index)\n");
    for(i = 0; i < 16; i++)
      printf(" vresult_ch[%d] = %d, expected_vresult_ch[%d] = %d\n",
	     i, vresult_ch[i], i, expected_vresult_ch[i]);
#else
    abort();
#endif
  }

  index = 4;
  src_va_sh = (vector unsigned short) { 0, 1, 2, 3, 4, 5, 6, 7 };
  src_vb_sh = (vector unsigned short) { 10, 11, 12, 13, 14, 15, 16, 17 };
  vresult_sh = (vector unsigned short) { 0, 0, 0, 0, 0, 0, 0, 0 };
  expected_vresult_sh = (vector unsigned short) { 0, 1, 2, 3, 4, 14, 6, 7 };
						 
  vresult_sh = vec_inserth (src_vb_sh, src_va_sh, index);

  if (!vec_all_eq (vresult_sh,  expected_vresult_sh)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_vb_sh, src_va_sh, index)\n");
    for(i = 0; i < 8; i++)
      printf(" vresult_sh[%d] = %d, expected_vresult_sh[%d] = %d\n",
	     i, vresult_sh[i], i, expected_vresult_sh[i]);
#else
    abort();
#endif
  }

  index = 8;
  src_va_int = (vector unsigned int) { 0, 1, 2, 3 };
  src_vb_int = (vector unsigned int) { 10, 11, 12, 13 };
  vresult_int = (vector unsigned int) { 0, 0, 0, 0 };
  expected_vresult_int = (vector unsigned int) { 0, 12, 2, 3 };
						 
  vresult_int = vec_inserth (src_vb_int, src_va_int, index);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_inserth (src_vb_int, src_va_int, index)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }
  return 0;
}

/* { dg-final { scan-assembler {\mvinsblx\M} } } */
/* { dg-final { scan-assembler {\mvinshlx\M} } } */
/* { dg-final { scan-assembler {\mvinswlx\M} } } */
/* { dg-final { scan-assembler {\mvinsdlx\M} } } */
/* { dg-final { scan-assembler {\mvinsbvlx\M} } } */
/* { dg-final { scan-assembler {\mvinshvlx\M} } } */
/* { dg-final { scan-assembler {\mvinswvlx\M} } } */

/* { dg-final { scan-assembler {\mvinsbrx\M} } } */
/* { dg-final { scan-assembler {\mvinshrx\M} } } */
/* { dg-final { scan-assembler {\mvinswrx\M} } } */
/* { dg-final { scan-assembler {\mvinsdrx\M} } } */
/* { dg-final { scan-assembler {\mvinsbvrx\M} } } */
/* { dg-final { scan-assembler {\mvinshvrx\M} } } */
/* { dg-final { scan-assembler {\mvinswvrx\M} } } */

