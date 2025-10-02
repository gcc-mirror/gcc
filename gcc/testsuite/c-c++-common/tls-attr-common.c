/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fdump-ipa-whole-program" } */
/* Add -fPIE or -mno-direct-extern-access to disable direct access to
   external symbol from executable.  */
/* { dg-additional-options "-fPIE" { target { ! { i?86-*-* x86_64-*-* } } } } */
/* { dg-additional-options "-mno-direct-extern-access" { target { i?86-*-* x86_64-*-* } } } */

__attribute__((common))
__thread int i;

int *
foo (void)
{
  return &i;
}

/* tls_model should be tls-initial-exec due to common attribute.  */
/* { dg-final { scan-ipa-dump "Varpool flags: tls-initial-exec" "whole-program" } } */
