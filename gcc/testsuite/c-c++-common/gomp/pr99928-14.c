/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int j00a, j00b, j01a, j01b, j02a, j02b, j03a, j03b, j04a, j04b;
int j06a, j06b, j07a, j07b, j08a, j08b, j09a, j09b, j10a, j10b;

void
foo (void)
{
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j00a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j00a\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j00a\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j00a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j00b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j00b\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j00b\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j00b\\)" "gimple" } } */
  #pragma omp distribute parallel for simd collapse(2) default(none)
  for (j00a = 0; j00a < 64; j00a++)
    for (j00b = 0; j00b < 4; j00b++)
      ;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j01a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j01a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j01b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j01b\\)" "gimple" } } */
  #pragma omp distribute simd collapse(2)
  for (j01a = 0; j01a < 64; j01a++)
    for (j01b = 0; j01b < 4; j01b++)
      ;
}

void
bar (void)
{
  int j05a, j05b, j11a, j11b, j12a, j12b;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j02a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j02b\\)" "gimple" } } */
  #pragma omp for simd collapse(2)
  for (j02a = 0; j02a < 64; j02a++)
    for (j02b = 0; j02b < 4; j02b++)
      ;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j03a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j03a\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j03a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j03a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j03b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j03b\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j03b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j03b\\)" "gimple" } } */
  #pragma omp master taskloop simd collapse(2) default(none)
  for (j03a = 0; j03a < 64; j03a++)
    for (j03b = 0; j03b < 4; j03b++)
      ;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j04a\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j04a\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j04a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j04b\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j04b\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j04b\\)" "gimple" } } */
  #pragma omp parallel for simd collapse(2) default(none)
  for (j04a = 0; j04a < 64; j04a++)
    for (j04b = 0; j04b < 4; j04b++)
      ;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j05a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j05a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j05a\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j05a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j05a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j05b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(j05b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j05b\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j05b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j05b\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd collapse(2) default(none)
  for (j05a = 0; j05a < 64; j05a++)
    for (j05b = 0; j05b < 4; j05b++)
      ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j06a" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j06a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j06a\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j06a\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j06a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j06b" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j06b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j06b\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j06b\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j06b\\)" "gimple" } } */
  #pragma omp target parallel for simd collapse(2) default(none) defaultmap(none)
  for (j06a = 0; j06a < 64; j06a++)
    for (j06b = 0; j06b < 4; j06b++)
      ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j07a" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j07a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j07a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j07b" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j07b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j07b\\)" "gimple" } } */
  #pragma omp target simd collapse(2) defaultmap(none)
  for (j07a = 0; j07a < 64; j07a++)
    for (j07b = 0; j07b < 4; j07b++)
      ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j08a" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j08a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j08a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j08a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j08a\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j08a\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j08a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j08b" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j08b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j08b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j08b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j08b\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j08b\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j08b\\)" "gimple" } } */
  #pragma omp target teams distribute parallel for simd collapse(2) default(none) defaultmap(none)
  for (j08a = 0; j08a < 64; j08a++)
    for (j08b = 0; j08b < 4; j08b++)
      ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j09a" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j09a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j09a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j09a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j09a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j09b" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j09b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j09b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j09b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j09b\\)" "gimple" } } */
  #pragma omp target teams distribute simd collapse(2) default(none) defaultmap(none)
  for (j09a = 0; j09a < 64; j09a++)
    for (j09b = 0; j09b < 4; j09b++)
      ;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j10a\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j10a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j10a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(j10b\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(j10b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j10b\\)" "gimple" } } */
  #pragma omp taskloop simd collapse(2) default(none)
  for (j10a = 0; j10a < 64; j10a++)
    for (j10b = 0; j10b < 4; j10b++)
      ;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j11a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j11a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j11a\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j11a\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j11a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j11b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j11b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(j11b\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(j11b\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j11b\\)" "gimple" } } */
  #pragma omp teams distribute parallel for simd collapse(2) default(none)
  for (j11a = 0; j11a < 64; j11a++)
    for (j11b = 0; j11b < 4; j11b++)
      ;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j12a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j12a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j12a\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j12b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j12b\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j12b\\)" "gimple" } } */
  #pragma omp teams distribute simd collapse(2) default(none)
  for (j12a = 0; j12a < 64; j12a++)
    for (j12b = 0; j12b < 4; j12b++)
      ;
}
