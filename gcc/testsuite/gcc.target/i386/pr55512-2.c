/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (int x)
{
  asm goto ("" : : "r" (x), "r" (x + 1), "r" (x + 2), "r" (x + 3), /* { dg-error "operand has impossible constraints" } */
	    "r" (x + 4), "r" (x + 5), "r" (x + 6), "r" (x + 7),
	    "r" (x + 8), "r" (x + 9), "r" (x + 10), "r" (x + 11),
	    "r" (x + 12), "r" (x + 13), "r" (x + 14), "r" (x + 15) : : lab);
 lab:
  return 0;
}
