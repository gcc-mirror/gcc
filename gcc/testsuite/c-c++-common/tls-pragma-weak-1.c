/* PR c/125418 */
/* { dg-do compile { target tls_native } } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -fdump-ipa-whole-program" } */

/* A hidden TLS variable made weak by #pragma weak may be undefined, so it
   does not bind locally and must not get a local TLS access model, whether
   the pragma follows or precedes the declaration.  */

__attribute__((visibility("hidden")))
extern __thread int t1;
#pragma weak t1

#pragma weak t2
__attribute__((visibility("hidden")))
extern __thread int t2;

int *f1 (void) { return &t1; }
int *f2 (void) { return &t2; }

/* { dg-final { scan-ipa-dump-not "Varpool flags: tls-local" "whole-program" } } */
