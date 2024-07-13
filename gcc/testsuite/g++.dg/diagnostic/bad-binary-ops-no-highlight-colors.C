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
  [m[K [01;31m[Kerror: [m[Kno match for '[01m[Koperator+[m[K' (operand types are '[01m[KS[m[K' {aka '[01m[Ks[m[K'} and '[01m[KT[m[K' {aka '[01m[Kt[m[K'})
     { dg-end-multiline-output "" } */

  /* { dg-begin-multiline-output "" }
   return [32m[Kcallee_4a ()[m[K [01;31m[K+[m[K [34m[Kcallee_4b ()[m[K;
          [32m[K~~~~~~~~~~~~[m[K [01;31m[K^[m[K [34m[K~~~~~~~~~~~~[m[K
                    [32m[K|[m[K              [34m[K|[m[K
                    [32m[KS {aka s}[m[K      [34m[KT {aka t}[m[K
     { dg-end-multiline-output "" } */

  /* { dg-prune-output "In function" } */
  /* { dg-prune-output "bad-binary-ops-no-highlight-colors.C" } */
}
