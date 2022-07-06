/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fPIC -fdump-ipa-whole-program" } */


#pragma GCC visibility push(hidden)

// tls_model should be local-dynamic due to a pragma
__thread int x;

#pragma GCC visibility pop

void reference() { x++; }

/* { dg-final { scan-ipa-dump "Varpool flags: tls-local-dynamic" "whole-program" } } */
