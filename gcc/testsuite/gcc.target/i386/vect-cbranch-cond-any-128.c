/* Test cond_vec_cbranch_any<mode> for the 128-bit V_ANY_CBRANCH modes
   (V4SI, V2DI, V4SF, V2DF).  */
/* { dg-do compile } */
/* { dg-options "-Ofast -mtune=generic -mavx512f -mavx512vl -mavx512dq -mprefer-vector-width=128 -fno-schedule-insns -fno-reorder-blocks -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

int       a_int[5],    b_int[5],    c_int[5];
long long a_long[3],   b_long[3],   c_long[3];
float     a_float[5],  b_float[5],  c_float[5];
double    a_double[3], b_double[3], c_double[3];

/*
** f_v4si:
**	...
**	vpcmpd	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+
**	vpcmpd	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+\{%k[0-9]+\}
**	kortestb	%k[0-9]+, %k[0-9]+
**	jne	\.L[0-9]+
**	...
*/
void f_v4si (int d, int e)
{
  for (int i = 0; i < 5; i++)
    {
      b_int[i] += a_int[i];
      if (c_int[i] == e) continue;
      if (a_int[i] != d) break;
    }
}

/*
** f_v2di:
**	...
**	vpcmpq	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+
**	vpcmpq	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+\{%k[0-9]+\}
**	kortestb	%k[0-9]+, %k[0-9]+
**	jne	\.L[0-9]+
**	...
*/
void f_v2di (long long d, long long e)
{
  for (int i = 0; i < 3; i++)
    {
      b_long[i] += a_long[i];
      if (c_long[i] == e) continue;
      if (a_long[i] != d) break;
    }
}

/*
** f_v4sf:
**	...
**	vcmpps	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+
**	vcmpps	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+\{%k[0-9]+\}
**	kortestb	%k[0-9]+, %k[0-9]+
**	jne	\.L[0-9]+
**	...
*/
void f_v4sf (float d, float e)
{
  for (int i = 0; i < 5; i++)
    {
      b_float[i] += a_float[i];
      if (c_float[i] == e) continue;
      if (a_float[i] != d) break;
    }
}

/*
** f_v2df:
**	...
**	vcmppd	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+
**	vcmppd	\$4, %xmm[0-9]+, %xmm[0-9]+, %k[0-9]+\{%k[0-9]+\}
**	kortestb	%k[0-9]+, %k[0-9]+
**	jne	\.L[0-9]+
**	...
*/
void f_v2df (double d, double e)
{
  for (int i = 0; i < 3; i++)
    {
      b_double[i] += a_double[i];
      if (c_double[i] == e) continue;
      if (a_double[i] != d) break;
    }
}
