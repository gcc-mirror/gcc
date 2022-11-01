/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fPIC -fdump-ipa-whole-program" } */

// tls_model should be global-dynamic due to explicitly specified attribute
__attribute__((tls_model("global-dynamic")))
__thread int x;

void reference() { x++; }

/* { dg-final { scan-ipa-dump "Varpool flags: tls-global-dynamic" "whole-program" } } */
