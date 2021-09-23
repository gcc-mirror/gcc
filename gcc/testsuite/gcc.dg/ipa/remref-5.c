/* { dg-do compile } */
/* { dg-options "-O3 -fno-early-inlining -fdump-ipa-cp-details -fdump-tree-optimized"  }  */

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

double last_value;

static void bar(double *ptr)
{
  last_value = *ptr;
  for (unsigned i = 0; i < 200; i++)
    foo (ptr);
}

void entry()
{
  bar (&global);
}

void decoy(double *ptr)
{
  bar (ptr);
}


/* { dg-final { scan-ipa-dump "Removed a reference"  "cp"  } } */
/* { dg-final { scan-ipa-dump "replaced it with LOAD"  "cp"  } } */
/* { dg-final { scan-tree-dump-times "builtin_exp" 1 "optimized"  } } */
