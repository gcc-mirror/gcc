/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */

short
core_bench_list (int N) {

  short a = 0;
  for (int i = 0; i < 4; i++) {
    if (i > N) {
      a++;
    }
  }
  return a * 4;
}
