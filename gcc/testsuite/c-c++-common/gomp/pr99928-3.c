/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int l00, l01, l02, l03, l04, l07, l08, l09;
int l10, l11;

void
bar (void)
{
  int l05 = 0, l06 = 0;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*firstprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } */
  #pragma omp for simd firstprivate (l00) lastprivate (l00)
  for (int i = 0; i < 64; i++)
    l00 = i;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  #pragma omp master taskloop firstprivate (l01) lastprivate (l01) default(none)
  for (int i = 0; i < 64; i++)
    l01 = i;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l02\\)" "gimple" } } */
  #pragma omp master taskloop simd firstprivate (l02) lastprivate (l02) default(none)
  for (int i = 0; i < 64; i++)
    l02 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l03\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } *//* FIXME.  */
  #pragma omp parallel for firstprivate (l03) lastprivate (l03) default(none)
  for (int i = 0; i < 64; i++)
    l03 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } */
  #pragma omp parallel for simd firstprivate (l04) lastprivate (l04) default(none)
  for (int i = 0; i < 64; i++)
    l04 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l05\\)" "gimple" } } */
  #pragma omp parallel master taskloop firstprivate (l05) lastprivate (l05) default(none)
  for (int i = 0; i < 64; i++)
    l05 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l06\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd firstprivate (l06) lastprivate (l06) default(none)
  for (int i = 0; i < 64; i++)
    l06 = i;
  /* FIXME: OpenMP 5.0/5.1 broken here, conceptually it should be shared on parallel and
     firstprivate+lastprivate on sections, in GCC implementation we put firstprivate+lastprivate
     on parallel for historic reasons, but OpenMP 5.0/5.1 mistakenly say firstprivate
     should be on parallel and lastprivate on sections.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*firstprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*firstprivate\\(l07\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*lastprivate\\(l07\\)" "gimple" } } */
  #pragma omp parallel sections firstprivate (l07) lastprivate (l07) default(none)
  {
    l07 = 1;
    #pragma omp section
    l07 = 2;
  }
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l08" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l08\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target parallel for firstprivate (l08) lastprivate (l08) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l08 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l09" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } */
  #pragma omp target parallel for simd firstprivate (l09) lastprivate (l09) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l09 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l10" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l10\\)" "gimple" } } */
  #pragma omp target simd firstprivate (l10) lastprivate (l10) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l10 = i;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*firstprivate\\(l11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp simd\[^\n\r]*firstprivate\\(l11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } */
 #pragma omp taskloop simd firstprivate (l11) lastprivate (l11) default(none)
  for (int i = 0; i < 64; i++)
    l11 = i;
}
