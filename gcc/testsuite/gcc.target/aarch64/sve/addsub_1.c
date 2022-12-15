/* { dg-do compile } */
/* { dg-options "-Ofast" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

/*
** f1:
** ...
** 	fneg	z[0-9]+.d, p[0-9]+/m, z[0-9]+.d
** 	fadd	z[0-9]+.s, z[0-9]+.s, z[0-9]+.s
** ...
*/
void f1 (float *restrict a, float *restrict b, float *res, int n)
{
   for (int i = 0; i < (n & -4); i+=2)
    {
      res[i+0] = a[i+0] + b[i+0];
      res[i+1] = a[i+1] - b[i+1];
    }
}

/* 
** d1:
** ...
** 	fneg	z[0-9]+.s, p[0-9]+/m, z[0-9]+.s
** 	fadd	z[0-9]+.h, z[0-9]+.h, z[0-9]+.h
** ...
*/ 
void d1 (_Float16 *restrict a, _Float16 *restrict b, _Float16 *res, int n)
{
   for (int i = 0; i < (n & -8); i+=2)
    {
      res[i+0] = a[i+0] + b[i+0];
      res[i+1] = a[i+1] - b[i+1];
    }
}

/*
** e1:
** ...
** 	fsub	z[0-9]+.d, z[0-9]+.d, z[0-9]+.d
** 	movprfx	z[0-9]+.d, p[0-9]+/m, z[0-9]+.d
** 	fadd	z[0-9]+.d, p[0-9]+/m, z[0-9]+.d, z[0-9]+.d
** ...
*/
void e1 (double *restrict a, double *restrict b, double *res, int n)
{
   for (int i = 0; i < (n & -4); i+=2)
    {
      res[i+0] = a[i+0] + b[i+0];
      res[i+1] = a[i+1] - b[i+1];
    }
}
