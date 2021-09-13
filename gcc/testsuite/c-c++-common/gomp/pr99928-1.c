/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int f00, f01, f02, f03, f04, f05, f06, f07, f08, f09;
int f12, f13, f14, f15, f16, f17, f18, f19;
int f20, f21, f22, f23, f24, f25, f26, f27, f28, f29;

void
foo (void)
{
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f00\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f00\\)" "gimple" } } *//* FIXME.  */
  #pragma omp distribute parallel for firstprivate (f00) default(none)
  for (int i = 0; i < 64; i++)
    f00++;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f01\\)" "gimple" } } */
  #pragma omp distribute parallel for simd firstprivate (f01) default(none)
  for (int i = 0; i < 64; i++)
    f01++;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*firstprivate\\(f02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f02\\)" "gimple" } } */
  #pragma omp distribute simd firstprivate (f02)
  for (int i = 0; i < 64; i++)
    f02++;
}

void
bar (void)
{
  int f10 = 0, f11 = 0;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(f03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f03\\)" "gimple" } } */
  #pragma omp for simd firstprivate (f03)
  for (int i = 0; i < 64; i++)
    f03++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f04\\)" "gimple" } } */
  #pragma omp master taskloop firstprivate (f04) default(none)
  for (int i = 0; i < 64; i++)
    f04++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f05\\)" "gimple" } } */
  #pragma omp master taskloop simd firstprivate (f05) default(none)
  for (int i = 0; i < 64; i++)
    f05++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f06\\)" "gimple" } } *//* FIXME.  */
  #pragma omp parallel for firstprivate (f06) default(none)
  for (int i = 0; i < 64; i++)
    f06++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f07\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f07\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f07\\)" "gimple" } } */
  #pragma omp parallel for simd firstprivate (f07) default(none)
  for (int i = 0; i < 64; i++)
    f07++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f08\\)" "gimple" } } */
  #pragma omp parallel loop firstprivate (f08) default(none)
  for (int i = 0; i < 64; i++)
    f08++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f09\\)" "gimple" } } */
  #pragma omp parallel master firstprivate (f09) default(none)
  f09++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f10\\)" "gimple" } } */
  #pragma omp parallel master taskloop firstprivate (f10) default(none)
  for (int i = 0; i < 64; i++)
    f10++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(f11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f11\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd firstprivate (f11) default(none)
  for (int i = 0; i < 64; i++)
    f11++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f12\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*firstprivate\\(f12\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*firstprivate\\(f12\\)" "gimple" } } */
  #pragma omp parallel sections firstprivate (f12) default(none)
  {
    f12++;
    #pragma omp section
    f12++;
  }
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f13\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f13\\)" "gimple" } } */
  #pragma omp target parallel firstprivate (f13) default(none) defaultmap(none)
  f13++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f14\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f14\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f14\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target parallel for firstprivate (f14) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f14++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f15\\)" "gimple" } } */
  #pragma omp target parallel for simd firstprivate (f15) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f15++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f16\\)" "gimple" } } */
  #pragma omp target parallel loop firstprivate (f16) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f16++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f17\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f17\\)" "gimple" } } */
  #pragma omp target teams firstprivate (f17) default(none) defaultmap(none)
  f17++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f18\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f18\\)" "gimple" } } *//* FIXME: This should be on distribute instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f18\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target teams distribute firstprivate (f18) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f18++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } *//* FIXME: This should be on distribute instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f19\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target teams distribute parallel for firstprivate (f19) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f19++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f20\\)" "gimple" } } */
  #pragma omp target teams distribute parallel for simd firstprivate (f20) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f20++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f21\\)" "gimple" } } */
  #pragma omp target teams distribute simd firstprivate (f21) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f21++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f22\\)" "gimple" } } *//* NOTE: This is an implementation detail.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f22\\)" "gimple" } } */
  #pragma omp target teams loop firstprivate (f22) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f22++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*firstprivate\\(f23\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f23\\)" "gimple" } } */
  #pragma omp target simd firstprivate (f23) defaultmap(none)
  for (int i = 0; i < 64; i++)
    f23++;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(f24\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f24\\)" "gimple" } } */
  #pragma omp taskloop simd firstprivate (f24) default(none)
  for (int i = 0; i < 64; i++)
    f24++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f25\\)" "gimple" } } *//* FIXME: This should be on distribute instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f25\\)" "gimple" } } *//* FIXME.  */
  #pragma omp teams distribute firstprivate (f25) default(none)
  for (int i = 0; i < 64; i++)
    f25++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } *//* FIXME: This should be on distribute instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f26\\)" "gimple" } } *//* FIXME.  */
  #pragma omp teams distribute parallel for firstprivate (f26) default(none)
  for (int i = 0; i < 64; i++)
    f26++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f27\\)" "gimple" } } */
  #pragma omp teams distribute parallel for simd firstprivate (f27) default(none)
  for (int i = 0; i < 64; i++)
    f27++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f28\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f28\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f28\\)" "gimple" } } */
  #pragma omp teams distribute simd firstprivate (f28) default(none)
  for (int i = 0; i < 64; i++)
    f28++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(f29\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*firstprivate\\(f29\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(f29\\)" "gimple" } } *//* NOTE: This is an implementation detail.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(f29\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(f29\\)" "gimple" } } */
  #pragma omp teams loop firstprivate (f29) default(none)
  for (int i = 0; i < 64; i++)
    f29++;
}
