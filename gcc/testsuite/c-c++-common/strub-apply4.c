/* { dg-do compile } */
/* { dg-options "-O2 -fstrub=strict -fdump-ipa-strubm" } */
/* { dg-require-effective-target strub } */

/* Check that implicit enabling of strub mode selects internal strub when the
   function uses __builtin_apply_args, that prevents the optimization to
   at-calls mode.  */

int __attribute__ ((__strub__)) var;

static inline void
apply_args (int i, int j, double d)
{
  var++;
  __builtin_apply_args ();
}

void f() {
  apply_args (1, 2, 3);
}

/* { dg-final { scan-ipa-dump-times "strub \[(\]internal\[)\]" 1 "strubm" } } */
