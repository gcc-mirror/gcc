/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fdump-ipa-cp-details -fdump-tree-optimized"  }  */

static double global = 0.0;

double foo_temp5;

static void foo(double *ptr) {
  static double abcd;
  double v, exp_res;
  v = *ptr;
  exp_res = __builtin_exp(v);
  foo_temp5 = exp_res * abcd;
  abcd += foo_temp5;
}

void entry()
{
  foo(&global);
}

/* { dg-final { scan-ipa-dump "Removed a reference"  "cp"  } } */
/* { dg-final { scan-tree-dump-not "builtin_exp"  "optimized"  } } */
