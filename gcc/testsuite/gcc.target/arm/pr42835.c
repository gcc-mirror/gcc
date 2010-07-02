/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb2_ok } */

int foo(int *p, int i)
{
  return( (i < 0 && *p == 1)
	  || (i > 0 && *p == 2) );
}

/* { dg-final { scan-assembler-times "movne\[\\t \]*r.,\[\\t \]*#" 1 } } */
/* { dg-final { scan-assembler-times "moveq\[\\t \]*r.,\[\\t \]*#" 1 } } */
