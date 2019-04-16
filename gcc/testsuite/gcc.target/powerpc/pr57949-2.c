/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc_elfv2 } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -mcompat-align-parm" } */

/* Verify that vs is not 16-byte aligned with -mcompat-align-parm.  */

typedef float v4sf __attribute__ ((vector_size (16)));
struct s { long m; v4sf v; };
long n;
v4sf ve;

void pr57949 (long d1, long d2, long d3, long d4, long d5, long d6,
	      long d7, long d8, long d9, struct s vs) {
  n = vs.m;
  ve = vs.v;
}

/* { dg-final { scan-assembler "ld .\*,136\\(1\\)" } } */
/* { dg-final { scan-assembler "ld .\*,120\\(1\\)" } } */
