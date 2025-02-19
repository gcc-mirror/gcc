/* { dg-do compile } */
/* { dg-options "-Ofast -mcpu=neoverse-n2 --param aarch64-vect-unroll-limit=2" } */
/* { dg-final { scan-assembler {, #?8.0e\+0} } } */
/* { dg-final { scan-assembler-not {\tmov\tv} } } */

/* Insert the induction IV updates before the exit condition, rather than
   at the start of the loop body.

   before (suggested_unroll_factor=2):
	  fmov    s30, 8.0e+0
	  fmov    s31, 4.0e+0
	  dup     v27.4s, v30.s[0]
	  dup     v28.4s, v31.s[0]
     .L6:
	  mov     v30.16b, v31.16b
	  fadd    v31.4s, v31.4s, v27.4s
	  fadd    v29.4s, v30.4s, v28.4s
	  stp     q30, q29, [x0]
	  add     x0, x0, 32
	  cmp     x1, x0
	  bne     .L6

   after:
	  fmov    s31, 8.0e+0
	  fmov    s29, 4.0e+0
	  dup     v31.4s, v31.s[0]
	  dup     v29.4s, v29.s[0]
     .L2:
	  fadd    v30.4s, v0.4s, v29.4s
	  stp     q0, q30, [x0], 32
	  fadd    v0.4s, v0.4s, v31.4s
	  cmp     x1, x0
	  bne     .L2  */

void
foo2 (float *arr, float freq, float step)
{
  for (int i = 0; i < 1024; i++)
    {
      arr[i] = freq;
      freq += step;
    }
}
