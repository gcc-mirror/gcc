/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc -mabi=lp64d -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

int t1(int);

int t2(int x)
{
  char *p = __builtin_alloca (x);
  x = t1 (x);
  return p[x];
}


/* This test has a variable sized alloca.  It requires 3 probes.
   One in the loop, one for the residual, one for when it's < 1024 and one for
   when it's not.

   The form can change quite a bit so we just check for three
   probes without looking at the actual address.  */
/* { dg-final { scan-assembler-times {sd\tzero,} 3 } } */
