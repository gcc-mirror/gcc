/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */
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

   The form can change quite a bit so we just check for two
   probes without looking at the actual address.  */
/* { dg-final { scan-assembler-times "str\\txzr," 3 } } */
