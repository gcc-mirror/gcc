/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fpic -fdump-ipa-whole-program" } */

__attribute__ ((tls_model ("local-exec"))) __thread int i;

int *
foo (void)
{
  return &i;
}

/* tls_model should be local-exec due to tls_model attribute.  */
/* { dg-final { scan-ipa-dump "Varpool flags: tls-local-exec" "whole-program" } } */
