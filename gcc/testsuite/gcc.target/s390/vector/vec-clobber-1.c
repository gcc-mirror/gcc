/* { dg-do run { target { s390*-*-* } } } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* For FP zero checks we use the ltdbr instruction.  Since this is an
   load and test it actually writes the FPR.  Whenever an FPR gets
   written the rest of the overlapping VR is clobbered.  */
typedef double __attribute__((vector_size(16))) v2df;

v2df a = { 1.0, 2.0 };

extern void abort (void);

void __attribute__((noinline))
foo (v2df a)
{
  v2df b = { 1.0, 3.0 };

  b -= a;

  /* Take away all the VRs not overlapping with FPRs.  */
  asm volatile ("" : : :
		"v16","v17","v18","v19",
		"v20","v21","v22","v23",
		"v24","v25","v26","v27",
		"v28","v29","v30","v31");
  if (b[0] != 0.0) /* ltdbr */
    abort ();
  if (b[1] != 1.0)
    abort ();
}

int
main ()
{
  foo (a);
  return 0;
}
