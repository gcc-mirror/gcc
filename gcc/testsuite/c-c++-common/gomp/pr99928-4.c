/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int l00, l01, l05, l06, l07, l08;

void
bar (void)
{
  int l02 = 0, l03 = 0, l04 = 0;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l00:1\\)" "gimple" } } */
  #pragma omp for simd linear (l00)
  for (int i = 0; i < 64; i++)
    l00++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l01:1\\)" "gimple" } } */
  #pragma omp master taskloop simd linear (l01) default(none)
  for (int i = 0; i < 64; i++)
    l01++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*linear\\(l02:1\\)" "gimple" } } */
  #pragma omp parallel for linear (l02) default(none)
  for (int i = 0; i < 64; i++)
    l02++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l03:1\\)" "gimple" } } */
  #pragma omp parallel for simd linear (l03) default(none)
  for (int i = 0; i < 64; i++)
    l03++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l04:1\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd linear (l04) default(none)
  for (int i = 0; i < 64; i++)
    l04++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l05" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*linear\\(l05:1\\)" "gimple" } } */
  #pragma omp target parallel for linear (l05) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l05++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l06" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l06:1\\)" "gimple" } } */
  #pragma omp target parallel for simd linear (l06) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l06++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l07" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l07:1\\)" "gimple" } } */
  #pragma omp target simd linear (l07) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l07++;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(l08:1\\)" "gimple" } } */
  #pragma omp taskloop simd linear (l08) default(none)
  for (int i = 0; i < 64; i++)
    l08++;
}
