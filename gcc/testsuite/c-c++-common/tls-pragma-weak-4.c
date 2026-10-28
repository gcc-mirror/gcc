/* PR c/125418 */
/* { dg-do compile { target tls_native } } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -fdump-ipa-whole-program" } */

/* As tls-pragma-weak-1.c and tls-pragma-weak-2.c, but with the variable
   redeclared after it has become weak.  The model is chosen for the
   redeclaration before it is merged with the weak declaration, so it must
   be recomputed then, however the variable was made weak.  */

__attribute__((visibility("hidden")))
extern __thread int t1;
#pragma weak t1
__attribute__((visibility("hidden")))
extern __thread int t1;

#pragma weak t2
__attribute__((visibility("hidden")))
extern __thread int t2;
__attribute__((visibility("hidden")))
extern __thread int t2;

__attribute__((visibility("hidden"), weak))
extern __thread int t3;
__attribute__((visibility("hidden")))
extern __thread int t3;

__attribute__((visibility("hidden"), tls_model("initial-exec")))
extern __thread int t4;
#pragma weak t4
__attribute__((visibility("hidden"), tls_model("initial-exec")))
extern __thread int t4;

int *f1 (void) { return &t1; }
int *f2 (void) { return &t2; }
int *f3 (void) { return &t3; }
int *f4 (void) { return &t4; }

/* { dg-final { scan-ipa-dump-not "Varpool flags: tls-local" "whole-program" } } */
