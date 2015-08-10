/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O2 -fpic -fdump-rtl-loop2_invariant" } */
/* { dg-skip-if "-mcmodel=large, no support for -fpic" { aarch64-*-* }  { "-mcmodel=large" } { "" } } */

int cal (int, int);
__thread int tls_data;

int
foo (int bound)
{
  int i = 0;
  int sum = 0;

  for (i; i < bound; i++)
    sum = cal (sum, tls_data);

  return sum;
}

/* Insn sequences for TLS descriptor should be hoisted out of the loop.  */
/* { dg-final { scan-rtl-dump "Decided" "loop2_invariant" } } */
