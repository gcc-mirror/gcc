/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

void f1 (volatile float32x4_t *dest, volatile float32x4x4_t *src, int n)
{
  float32x4x4_t a5, a6, a7, a8, a9;
  int i;

  a5 = *src;
  a6 = *src;
  a7 = *src;
  a8 = *src;
  a9 = *src;
  while (n--)
    {
      for (i = 0; i < 8; i++)
	{
	  float32x4x4_t a0, a1, a2, a3, a4;

	  a0 = *src;
	  a1 = *src;
	  a2 = *src;
	  a3 = *src;
	  a4 = *src;
	  *src = a0;
	  *dest = a0.val[0];
	  *dest = a0.val[3];
	  *src = a1;
	  *dest = a1.val[0];
	  *dest = a1.val[3];
	  *src = a2;
	  *dest = a2.val[0];
	  *dest = a2.val[3];
	  *src = a3;
	  *dest = a3.val[0];
	  *dest = a3.val[3];
	  *src = a4;
	  *dest = a4.val[0];
	  *dest = a4.val[3];
	}
      *src = a5;
      *dest = a5.val[0];
      *dest = a5.val[3];
      *src = a6;
      *dest = a6.val[0];
      *dest = a6.val[3];
      *src = a7;
      *dest = a7.val[0];
      *dest = a7.val[3];
      *src = a8;
      *dest = a8.val[0];
      *dest = a8.val[3];
      *src = a9;
      *dest = a9.val[0];
      *dest = a9.val[3];
    }
}
