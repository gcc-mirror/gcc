/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls } */
/* { dg-options "-g -fPIC -mtls-dialect=gnu" } */

__thread int i;

void
foo(void)
{
  i = 0;
}

/* { dg-final { scan-assembler "data16\[ \t\]*leaq" } } */
/* { dg-final { scan-assembler-not "\.byte\[ \t\]*0x66\n\[ \t\]*leaq" } } */
