/* { dg-do run } */
/* { dg-require-effective-target power10_hw } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

/* { dg-final { scan-assembler-times {\mvdivsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivuw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivud\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivesw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdiveuw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivesd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdiveud\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmoduw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodud\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhuw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulhud\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulld\M} 2 } } */

#include <stdint.h>
#include <stdio.h>
#include <math.h>
#include <altivec.h>

#define DEBUG 0

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main()
  {
    int i;
    vector int i_arg1, i_arg2;
    vector unsigned int u_arg1, u_arg2;
    vector long long int d_arg1, d_arg2;
    vector long long unsigned int ud_arg1, ud_arg2;
   
    vector int vec_i_expected, vec_i_result;
    vector unsigned int vec_u_expected, vec_u_result;
    vector long long int vec_d_expected, vec_d_result;
    vector long long unsigned int vec_ud_expected, vec_ud_result;
  
    /* Signed word divide */
    i_arg1 = (vector int){ 20, 40, 60, 80};
    i_arg2 = (vector int){ 2, 2, 2, 2};
    vec_i_expected = (vector int){10, 20, 30, 40};

    vec_i_result = vec_div (i_arg1, i_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_i_expected[i] != vec_i_result[i])
#ifdef DEBUG
           printf("ERROR vec_div signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_i_result[i],  i, vec_i_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned word divide */
    u_arg1 = (vector unsigned int){ 20, 40, 60, 80};
    u_arg2 = (vector unsigned int){ 2, 2, 2, 2};
    vec_u_expected = (vector unsigned int){10, 20, 30, 40};

    vec_u_result = vec_div (u_arg1, u_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_u_expected[i] != vec_u_result[i])
#ifdef DEBUG
           printf("ERROR vec_div unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_u_result[i],  i, vec_u_expected[i]);
#else
        abort();
#endif
      }

    /* Signed double word divide */
    d_arg1 = (vector long long){ 24, 68};
    d_arg2 = (vector long long){ 2, 2};
    vec_d_expected = (vector long long){12, 34};

    vec_d_result = vec_div (d_arg1, d_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_d_expected[i] != vec_d_result[i])
#ifdef DEBUG
	  printf("ERROR vec_div signed result[%d] = %d != "
		 "expected[%d] = %d\n",
		 i, vec_d_result[i],  i, vec_d_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned double word divide */
    ud_arg1 = (vector unsigned long long){ 24, 68};
    ud_arg2 = (vector unsigned long long){ 2, 2};
    vec_ud_expected = (vector unsigned long long){12, 34};

    vec_ud_result = vec_div (ud_arg1, ud_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_ud_expected[i] != vec_ud_result[i])
#ifdef DEBUG
           printf("ERROR vec_div unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_ud_result[i],  i, vec_ud_expected[i]);
#else
        abort();
#endif
      }

    /* Divide Extended signed word  result = (arg1 << 32)/arg2 */
    i_arg1 = (vector int){ 2, 4, 6, 8};
    i_arg2 = (vector int){ 2048, 2048, 2048, 2048};
    vec_i_expected = (vector int){4194304, 8388608, 12582912, 16777216};

    vec_i_result = vec_dive (i_arg1, i_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_i_expected[i] != vec_i_result[i])
#ifdef DEBUG
           printf("ERROR vec_dive signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_i_result[i],  i, vec_i_expected[i]);
#else
        abort();
#endif
      }

    /* Divide Extended unsigned word  result = (arg1 << 32)/arg2 */
    u_arg1 = (vector unsigned int){ 2, 4, 6, 8};
    u_arg2 = (vector unsigned int){ 2048, 2048, 2048, 2048};
    vec_u_expected = (vector unsigned int){4194304, 8388608,
					   12582912, 16777216};

    vec_u_result = vec_dive (u_arg1, u_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_u_expected[i] != vec_u_result[i])
#ifdef DEBUG
           printf("ERROR vec_dive unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_u_result[i],  i, vec_u_expected[i]);
#else
        abort();
#endif
      }

    /* Divide Extended double signed  esult = (arg1 << 64)/arg2 */
    d_arg1 = (vector long long int){ 2, 4};
    d_arg2 = (vector long long int){ 4294967296, 4294967296};

    vec_d_expected = (vector long long int){8589934592, 17179869184};

    vec_d_result = vec_dive (d_arg1, d_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_d_expected[i] != vec_d_result[i])
#ifdef DEBUG
           printf("ERROR vec_dive signed result[%d] = %lld != "
		  "expected[%d] = %lld\n",
		  i, vec_d_result[i],  i, vec_d_expected[i]);
#else
        abort();
#endif
      }

    /* Divide Extended double unsigned result = (arg1 << 64)/arg2 */
    ud_arg1 = (vector long long unsigned int){ 2, 4};
    ud_arg2 = (vector long long unsigned int){ 4294967296, 4294967296};

    vec_ud_expected = (vector long long unsigned int){8589934592,
						      17179869184};

    vec_ud_result = vec_dive (ud_arg1, ud_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_ud_expected[i] != vec_ud_result[i])
#ifdef DEBUG
           printf("ERROR vec_dive unsigned result[%d] = %lld != "
		  "expected[%d] = %lld\n",
		  i, vec_ud_result[i],  i, vec_ud_expected[i]);
#else
        abort();
#endif
      }

    /* Signed word modulo */
    i_arg1 = (vector int){ 23, 45, 61, 89};
    i_arg2 = (vector int){ 2, 2, 2, 2};
    vec_i_expected = (vector int){1, 1, 1, 1};

    vec_i_result = vec_mod (i_arg1, i_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_i_expected[i] != vec_i_result[i])
#ifdef DEBUG
           printf("ERROR vec_mod signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_i_result[i],  i, vec_i_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned word modulo */
    u_arg1 = (vector unsigned int){ 25, 41, 67, 86};
    u_arg2 = (vector unsigned int){ 3, 3, 3, 3};
    vec_u_expected = (vector unsigned int){1, 2, 1, 2};

    vec_u_result = vec_mod (u_arg1, u_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_u_expected[i] != vec_u_result[i])
#ifdef DEBUG
           printf("ERROR vec_mod unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_u_result[i],  i, vec_u_expected[i]);
#else
        abort();
#endif
      }

    /* Signed double word modulo */
    d_arg1 = (vector long long){ 24, 68};
    d_arg2 = (vector long long){ 7, 7};
    vec_d_expected = (vector long long){3, 5};

    vec_d_result = vec_mod (d_arg1, d_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_d_expected[i] != vec_d_result[i])
#ifdef DEBUG
           printf("ERROR vec_mod signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_d_result[i],  i, vec_d_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned double word modulo */
    ud_arg1 = (vector unsigned long long){ 24, 68};
    ud_arg2 = (vector unsigned long long){ 8, 8};
    vec_ud_expected = (vector unsigned long long){0, 4};

    vec_ud_result = vec_mod (ud_arg1, ud_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_ud_expected[i] != vec_ud_result[i])
#ifdef DEBUG
           printf("ERROR vecmod unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_ud_result[i],  i, vec_ud_expected[i]);
#else
        abort();
#endif
      }

    /* Signed word multiply high */
    i_arg1 = (vector int){ 2147483648, 2147483648, 2147483648, 2147483648 };
    i_arg2 = (vector int){ 2, 3, 4, 5};
    //    vec_i_expected = (vector int){-1, -2, -2, -3};
    vec_i_expected = (vector int){1, -2, -2, -3};

    vec_i_result = vec_mulh (i_arg1, i_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_i_expected[i] != vec_i_result[i])
#ifdef DEBUG
           printf("ERROR vec_mulh signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_i_result[i],  i, vec_i_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned word multiply high */
    u_arg1 = (vector unsigned int){ 2147483648, 2147483648,
				    2147483648, 2147483648 };
    u_arg2 = (vector unsigned int){ 4, 5, 6, 7 };
    vec_u_expected = (vector unsigned int){2, 2, 3, 3 };

    vec_u_result = vec_mulh (u_arg1, u_arg2);

    for (i = 0; i < 4; i++)
      {
        if (vec_u_expected[i] != vec_u_result[i])
#ifdef DEBUG
           printf("ERROR vec_mulh unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_u_result[i],  i, vec_u_expected[i]);
#else
        abort();
#endif
      }

    /* Signed double word multiply high */
    d_arg1 = (vector long long int){  2305843009213693951,
				      4611686018427387903 };
    d_arg2 = (vector long long int){ 12, 20 };
    vec_d_expected = (vector long long int){ 1, 4 };

    vec_d_result = vec_mulh (d_arg1, d_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_d_expected[i] != vec_d_result[i])
#ifdef DEBUG
           printf("ERROR vec_mulh signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_d_result[i],  i, vec_d_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned double word multiply high */
    ud_arg1 = (vector unsigned long long int){ 2305843009213693951,
					       4611686018427387903 };
    ud_arg2 = (vector unsigned long long int){ 32, 10 };
    vec_ud_expected = (vector unsigned long long int){ 3, 2 };

    vec_ud_result = vec_mulh (ud_arg1, ud_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_ud_expected[i] != vec_ud_result[i])
#ifdef DEBUG
           printf("ERROR vec_mulh unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_ud_result[i],  i, vec_ud_expected[i]);
#else
        abort();
#endif
      }

    /* Unsigned double word multiply low */
    ud_arg1 = (vector unsigned long long int){ 2048, 4096 };
    ud_arg2 = (vector unsigned long long int){ 2, 4 };
    vec_ud_expected = (vector unsigned long long int){ 4096, 16384 };

    vec_ud_result = vec_mul (ud_arg1, ud_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_ud_expected[i] != vec_ud_result[i])
#ifdef DEBUG
           printf("ERROR vec_mul unsigned result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_ud_result[i],  i, vec_ud_expected[i]);
#else
        abort();
#endif
      }

    /* Signed double word multiply low */
    d_arg1 = (vector signed long long int){ 2048, 4096 };
    d_arg2 = (vector signed long long int){ 2, 4 };
    vec_d_expected = (vector signed long long int){ 4096, 16384 };

    vec_d_result = vec_mul (d_arg1, d_arg2);

    for (i = 0; i < 2; i++)
      {
        if (vec_d_expected[i] != vec_d_result[i])
#ifdef DEBUG
           printf("ERROR vec_mul signed result[%d] = %d != "
		  "expected[%d] = %d\n",
		  i, vec_d_result[i],  i, vec_d_expected[i]);
#else
        abort();
#endif
      }
  }
