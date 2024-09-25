/* Verify that -fno-diagnostics-show-highlight-colors works.  */
/* { dg-options "-fdiagnostics-show-caret -fdiagnostics-color=always -fno-diagnostics-show-highlight-colors" } */

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
  [m[K [01;31m[Kerror: [m[Kinvalid operands to binary + (have '[01m[KS[m[K' {aka [m[K'[01m[Kstruct s[m[K'} and '[01m[KT[m[K' {aka [m[K'[01m[Kstruct t[m[K'})
     { dg-end-multiline-output "" } */

  /* { dg-begin-multiline-output "" }
   return [32m[Kcallee_4a ()[m[K [01;31m[K+[m[K [34m[Kcallee_4b ()[m[K;
          [32m[K~~~~~~~~~~~~[m[K [01;31m[K^[m[K [34m[K~~~~~~~~~~~~[m[K
          [32m[K|[m[K              [34m[K|[m[K
          [32m[K|[m[K              [34m[KT {aka struct t}[m[K
          [32m[KS {aka struct s}[m[K
     { dg-end-multiline-output "" } */

  /* { dg-prune-output "In function" } */
  /* { dg-prune-output "bad-binary-ops-no-highlight-colors.c" } */
}
