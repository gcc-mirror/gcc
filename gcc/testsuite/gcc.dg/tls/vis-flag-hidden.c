/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fPIC -fdump-ipa-whole-program -fvisibility=hidden" } */


// tls_model should be local-dynamic due to -fvisibility=hidden
__thread int x;

void reference() { x++; }

/* { dg-final { scan-ipa-dump "Varpool flags: tls-local-dynamic" "whole-program" } } */
