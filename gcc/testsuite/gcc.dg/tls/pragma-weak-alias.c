/* PR c/125418 */
/* { dg-do compile { target tls_native } } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-require-visibility "" } */
/* { dg-options "-O2 -fdump-ipa-visibility" } */

/* A hidden TLS variable that #pragma weak makes an alias of a variable
   defined here still binds locally, so the front end must not weaken its
   TLS access model the way it does for a weak undefined symbol.  Scan the
   early dump: later IPA passes would upgrade the model again.  The same
   holds when the alias is redeclared.  */

static __thread int local_def;

__attribute__((visibility("hidden")))
extern __thread int t;
#pragma weak t = local_def

static __thread int local_def2;

__attribute__((visibility("hidden")))
extern __thread int t2;
#pragma weak t2 = local_def2
__attribute__((visibility("hidden")))
extern __thread int t2;

int *f (void) { return &t; }
int *f2 (void) { return &t2; }

/* { dg-final { scan-ipa-dump-not "Varpool flags: tls-initial-exec" "visibility" } } */
