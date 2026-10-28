/* PR c/125418 */
/* { dg-do compile { target tls_native } } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -fdump-ipa-whole-program" } */

/* As tls-pragma-weak-1.c, but with an explicit tls_model attribute, which
   is how glibc declares _nl_current_LC_*.  The attribute is a lower bound:
   the model must stay initial-exec rather than be upgraded to local-exec.  */

__attribute__((visibility("hidden"), tls_model("initial-exec")))
extern __thread int t1;
#pragma weak t1

#pragma weak t2
__attribute__((visibility("hidden"), tls_model("initial-exec")))
extern __thread int t2;

int *f1 (void) { return &t1; }
int *f2 (void) { return &t2; }

/* { dg-final { scan-ipa-dump-times "Varpool flags: tls-initial-exec" 2 "whole-program" } } */
