/* Verify that if IP is saved to ensure stack alignment, we don't load
   it into sp.  */
/* { dg-do compile { target arm*-*-* strongarm*-*-* xscale*-*-*} } */
/* { dg-options "-O -mno-apcs-frame -mcpu=iwmmxt" } */
/* { dg-final { global compiler_flags; if ![string match "*-mthumb *" $compiler_flags] { scan-assembler "ldmfd\[ 	]sp!.*ip,\[ ]*pc" } } } */

/* This function uses all the call-saved registers, namely r4, r5, r6,
   r7, r8, r9, sl, fp.  Since we also save pc, that leaves an odd
   number of registers, and the compiler will push ip to align the
   stack.  Make sure that we restore ip into ip, not into sp as is
   done when using a frame pointer.  The -mno-apcs-frame option
   permits the frame pointer to be used as an ordinary register.  */
int
foo(int *a, int *b, int *c, int *d, int *tot)
{
  int i, j, k, l, m, n, o;

  *tot = 0;
  for (i = *a; i < *b; i += *c)
    for (j = *a; j < *b; j += *d)
      for (k = *a; k < *c; k += *d)
	for (l = *b; k < *c; k += *d)
	  for (m = *d; k < *c; k += *b)
	    *tot += i + j + k + l + m;
  return *tot;
}
