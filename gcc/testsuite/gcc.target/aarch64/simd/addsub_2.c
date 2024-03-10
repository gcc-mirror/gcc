/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-options "-Ofast" } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

#pragma GCC target "+nosve"

/* 
** f1:
** ...
**	fneg	v[0-9]+.2d, v[0-9]+.2d
**	fsub	v[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s
** ...
*/
void f1 (float *restrict a, float *restrict b, float *res, int n)
{
   for (int i = 0; i < (n & -4); i+=2)
    {
      res[i+0] = a[i+0] - b[i+0];
      res[i+1] = a[i+1] + b[i+1];
    }
}

/* 
** d1:
** ...
** 	fneg	v[0-9]+.4s, v[0-9]+.4s
** 	fsub	v[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h
** ...
*/
void d1 (_Float16 *restrict a, _Float16 *restrict b, _Float16 *res, int n)
{
   for (int i = 0; i < (n & -8); i+=2)
    {
      res[i+0] = a[i+0] - b[i+0];
      res[i+1] = a[i+1] + b[i+1];
    }
}

/* 
** e1:
** ...
** 	fsub	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
** 	fadd	v[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d
** 	ins	v[0-9]+.d\[1\], v[0-9]+.d\[1\]
** ...
*/
void e1 (double *restrict a, double *restrict b, double *res, int n)
{
   for (int i = 0; i < (n & -4); i+=2)
    {
      res[i+0] = a[i+0] - b[i+0];
      res[i+1] = a[i+1] + b[i+1];
    }
}
