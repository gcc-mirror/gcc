/* { dg-do compile { target { tls && pie } } } */
/* { dg-options "-O2 -fPIE -fdump-ipa-whole-program" } */

extern const int afoo[3];

__thread const int *pfoo __attribute__ ((tls_model ("initial-exec"))) = afoo;

const int **
ppfoo (void)
{
  return &pfoo;
}

/* tls_model should be local-exec due to -fPIE.  */
/* { dg-final { scan-ipa-dump "Varpool flags: initialized tls-local-exec" "whole-program" } } */
