/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int j00, j01, j02, j03, j04, j06, j07, j08, j09;
int j10;

void
foo (void)
{
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j00:1\\)" "gimple" } } */
  #pragma omp distribute parallel for simd linear (j00) default(none)
  for (j00 = 0; j00 < 64; j00++)
    ;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j01:1\\)" "gimple" } } */
  #pragma omp distribute simd linear (j01)
  for (j01 = 0; j01 < 64; j01++)
    ;
}

void
bar (void)
{
  int j05, j11, j12;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j02:1\\)" "gimple" } } */
  #pragma omp for simd linear (j02)
  for (j02 = 0; j02 < 64; j02++)
    ;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j03\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j03:1\\)" "gimple" } } */
  #pragma omp master taskloop simd linear (j03) default(none)
  for (j03 = 0; j03 < 64; j03++)
    ;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j04\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j04\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j04:1\\)" "gimple" } } */
  #pragma omp parallel for simd linear (j04) default(none)
  for (j04 = 0; j04 < 64; j04++)
    ;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j05\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j05:1\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd linear (j05) default(none)
  for (j05 = 0; j05 < 64; j05++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j06" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j06\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j06:1\\)" "gimple" } } */
  #pragma omp target parallel for simd linear (j06) default(none) defaultmap(none)
  for (j06 = 0; j06 < 64; j06++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j07" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j07:1\\)" "gimple" } } */
  #pragma omp target simd linear (j07) defaultmap(none)
  for (j07 = 0; j07 < 64; j07++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j08" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j08\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j08\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j08:1\\)" "gimple" } } */
  #pragma omp target teams distribute parallel for simd linear (j08) default(none) defaultmap(none)
  for (j08 = 0; j08 < 64; j08++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j09" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j09:1\\)" "gimple" } } */
  #pragma omp target teams distribute simd linear (j09) default(none) defaultmap(none)
  for (j09 = 0; j09 < 64; j09++)
    ;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j10\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j10:1\\)" "gimple" } } */
  #pragma omp taskloop simd linear (j10) default(none)
  for (j10 = 0; j10 < 64; j10++)
    ;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j11\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j11\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j11:1\\)" "gimple" } } */
  #pragma omp teams distribute parallel for simd linear (j11) default(none)
  for (j11 = 0; j11 < 64; j11++)
    ;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j12\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j12\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*linear\\(j12:1\\)" "gimple" } } */
  #pragma omp teams distribute simd linear (j12) default(none)
  for (j12 = 0; j12 < 64; j12++)
    ;
}
