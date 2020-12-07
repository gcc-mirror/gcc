/* { dg-do compile } */
/* { dg-options "-O2 -march=z900 -fstack-clash-protection" } */

extern void bar (char *);

void
foo ()
{
  char * mem = __builtin_alloca (20000);
  bar (mem);
}

/* For alloca a common code routine emits the probes.  Make sure the
   "probe_stack" expander is used in that case. We want to use mem
   compares instead of stores.  */
/* { dg-final { scan-assembler-times "cg\t" 5 { target s390_zarch } } } */
/* { dg-final { scan-assembler-times "c\t" 5 { target { ! s390_zarch } } } } */
