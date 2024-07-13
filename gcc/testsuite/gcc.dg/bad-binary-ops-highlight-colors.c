/* Verify that colorization affects both text within diagnostic messages
   and underlined ranges of quoted source, and that the types we use
   match up between them.
   Also implicitly verify that -fdiagnostics-show-highlight-colors is
   on by default.  */

/* { dg-options "-fdiagnostics-show-caret -fdiagnostics-color=always" } */

struct s {};
struct t {};
typedef struct s S;
typedef struct t T;

extern S callee_4a (void);
extern T callee_4b (void);

int test_4 (void)
{
  return callee_4a () + callee_4b ();

  /* { dg-begin-multiline-output "" }
  [m[K [01;31m[Kerror: [m[Kinvalid operands to binary + (have '[01m[K[01;32m[KS[m[K' {aka [m[K'[01m[K[01;32m[Kstruct s[m[K'}[m[K and '[01m[K[01;34m[KT[m[K' {aka [m[K'[01m[K[01;34m[Kstruct t[m[K'}[m[K)
     { dg-end-multiline-output "" } */

  /* { dg-begin-multiline-output "" }
   return [01;32m[Kc[m[K[01;32m[Ka[m[K[01;32m[Kl[m[K[01;32m[Kl[m[K[01;32m[Ke[m[K[01;32m[Ke[m[K[01;32m[K_[m[K[01;32m[K4[m[K[01;32m[Ka[m[K[01;32m[K [m[K[01;32m[K([m[K[01;32m[K)[m[K [01;31m[K+[m[K [01;34m[Kc[m[K[01;34m[Ka[m[K[01;34m[Kl[m[K[01;34m[Kl[m[K[01;34m[Ke[m[K[01;34m[Ke[m[K[01;34m[K_[m[K[01;34m[K4[m[K[01;34m[Kb[m[K[01;34m[K [m[K[01;34m[K([m[K[01;34m[K)[m[K;
          [01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K[01;32m[K~[m[K [01;31m[K^[m[K [01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K[01;34m[K~[m[K
          [01;32m[K|[m[K              [01;34m[K|[m[K
          [01;32m[K|[m[K              [01;34m[KT {aka struct t}[m[K
          [01;32m[KS {aka struct s}[m[K
     { dg-end-multiline-output "" } */

  /* { dg-prune-output "In function" } */
  /* { dg-prune-output "bad-binary-ops-highlight-colors.c" } */
}
