/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int r00, r01, r02, r03, r04, r05;
int r13, r14, r15, r16, r17, r18, r19;
int r20, r21, r22, r23, r24;

void
foo (void)
{
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r00\\)" "gimple" } } *//* FIXME.  */
  #pragma omp distribute parallel for reduction(+:r00) default(none)
  for (int i = 0; i < 64; i++)
    r00++;
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r01\\)" "gimple" } } */
  #pragma omp distribute parallel for simd reduction(+:r01) default(none)
  for (int i = 0; i < 64; i++)
    r01++;
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r02\\)" "gimple" } } */
  #pragma omp distribute simd reduction(+:r02)
  for (int i = 0; i < 64; i++)
    r02++;
}

void
bar (void)
{
  int r06 = 0, r07 = 0, r08 = 0, r09 = 0;
  int r10 = 0, r11 = 0, r12 = 0;
  int r25 = 0, r26 = 0, r27 = 0, r28 = 0, r29 = 0;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r03\\)" "gimple" } } */
  #pragma omp for simd reduction(+:r03)
  for (int i = 0; i < 64; i++)
    r03++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r04\\)" "gimple" } } */
  #pragma omp master taskloop reduction(+:r04) default(none)
  for (int i = 0; i < 64; i++)
    r04++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r05\\)" "gimple" } } */
  #pragma omp master taskloop simd reduction(+:r05) default(none)
  for (int i = 0; i < 64; i++)
    r05++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r06\\)" "gimple" } } *//* FIXME.  */
  #pragma omp parallel for reduction(+:r06) default(none)
  for (int i = 0; i < 64; i++)
    r06++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r07\\)" "gimple" } } */
  #pragma omp parallel for simd reduction(+:r07) default(none)
  for (int i = 0; i < 64; i++)
    r07++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r08\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r08\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp parallel loop reduction(+:r08) default(none)
  for (int i = 0; i < 64; i++)
    r08++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r09\\)" "gimple" } } */
  #pragma omp parallel master reduction(+:r09) default(none)
  r09++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r10\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r10\\)" "gimple" } } */
  #pragma omp parallel master taskloop reduction(+:r10) default(none)
  for (int i = 0; i < 64; i++)
    r10++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r11\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd reduction(+:r11) default(none)
  for (int i = 0; i < 64; i++)
    r11++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } } *//* FIXME: This should be on sections instead.  */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*reduction\\(\\+:r12\\)" "gimple" } } */
  #pragma omp parallel sections reduction(+:r12) default(none)
  {
    r12++;
    #pragma omp section
    r12++;
  }
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r13" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r13\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r13\\)" "gimple" } } */
  #pragma omp target parallel reduction(+:r13) default(none) defaultmap(none)
  r13++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r14" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r14\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r14\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r14\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target parallel for reduction(+:r14) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r14++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r15" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r15\\)" "gimple" } } */
  #pragma omp target parallel for simd reduction(+:r15) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r15++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r16" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r16\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r16\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target parallel loop reduction(+:r16) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r16++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r17" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r17\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r17\\)" "gimple" } } */
  #pragma omp target teams reduction(+:r17) default(none) defaultmap(none)
  r17++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r18" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r18\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r18\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r18\\)" "gimple" } } */
  #pragma omp target teams distribute reduction(+:r18) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r18++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r19" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r19\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target teams distribute parallel for reduction(+:r19) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r19++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r20" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r20\\)" "gimple" } } */
  #pragma omp target teams distribute parallel for simd reduction(+:r20) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r20++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r21" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r21\\)" "gimple" } } */
  #pragma omp target teams distribute simd reduction(+:r21) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r21++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r22" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(r22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r22\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r22\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target teams loop reduction(+:r22) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r22++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:r23" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r23\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r23\\)" "gimple" } } */
  #pragma omp target simd reduction(+:r23) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r23++;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:r24\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r24\\)" "gimple" } } */
  #pragma omp taskloop simd reduction(+:r24) default(none)
  for (int i = 0; i < 64; i++)
    r24++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r25\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r25\\)" "gimple" } } */
  #pragma omp teams distribute reduction(+:r25) default(none)
  for (int i = 0; i < 64; i++)
    r25++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r26\\)" "gimple" } } *//* FIXME.  */
  #pragma omp teams distribute parallel for reduction(+:r26) default(none)
  for (int i = 0; i < 64; i++)
    r26++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r27\\)" "gimple" } } */
  #pragma omp teams distribute parallel for simd reduction(+:r27) default(none)
  for (int i = 0; i < 64; i++)
    r27++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r28\\)" "gimple" } } */
  #pragma omp teams distribute simd reduction(+:r28) default(none)
  for (int i = 0; i < 64; i++)
    r28++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(r29\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(r29\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:r29\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp teams loop reduction(+:r29) default(none)
  for (int i = 0; i < 64; i++)
    r29++;
}
