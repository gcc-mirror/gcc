/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int *r00, *r01, *r02, *r03, *r04, *r05;
int *r13, *r14, *r15, *r16, *r17, *r18, *r19;
int *r20, *r21, *r22, *r23, *r24;
int *baz (void);

void
foo (void)
{
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r00 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r00 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r00 \\+ 4" "gimple" } } *//* FIXME.  */
  #pragma omp distribute parallel for reduction(+:r00[1:2]) default(none)
  for (int i = 0; i < 64; i++)
    r00[1]++;
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r01 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r01 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r01 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r01 \\+ 4" "gimple" } } */
  #pragma omp distribute parallel for simd reduction(+:r01[1:3]) default(none)
  for (int i = 0; i < 64; i++)
    r01[1]++;
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r02 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r02 \\+ 4" "gimple" } } */
  #pragma omp distribute simd reduction(+:r02[1:4])
  for (int i = 0; i < 64; i++)
    r02[1]++;
}

void
bar (void)
{
  int *r06 = baz (), *r07 = baz (), *r08 = baz (), *r09 = baz ();
  int *r10 = baz (), *r11 = baz (), *r12 = baz ();
  int *r25 = baz (), *r26 = baz (), *r27 = baz (), *r28 = baz (), *r29 = baz ();
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r03 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r03 \\+ 4" "gimple" } } */
  #pragma omp for simd reduction(+:r03[1:5])
  for (int i = 0; i < 64; i++)
    r03[1]++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r04 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r04 \\+ 4" "gimple" } } */
  #pragma omp master taskloop reduction(+:r04[1:6]) default(none)
  for (int i = 0; i < 64; i++)
    r04[1]++;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r05 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r05 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r05 \\+ 4" "gimple" } } */
  #pragma omp master taskloop simd reduction(+:r05[1:7]) default(none)
  for (int i = 0; i < 64; i++)
    r05[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r06 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r06 \\+ 4" "gimple" } } *//* FIXME.  */
  #pragma omp parallel for reduction(+:r06[1:8]) default(none)
  for (int i = 0; i < 64; i++)
    r06[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r07 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r07 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r07 \\+ 4" "gimple" } } */
  #pragma omp parallel for simd reduction(+:r07[1:9]) default(none)
  for (int i = 0; i < 64; i++)
    r07[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r08\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r08 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r08 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp parallel loop reduction(+:r08[1:10]) default(none)
  for (int i = 0; i < 64; i++)
    r08[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r09 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r09 \\+ 4" "gimple" } } */
  #pragma omp parallel master reduction(+:r09[1:11]) default(none)
  r09[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r10\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r10 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r10 \\+ 4" "gimple" } } */
  #pragma omp parallel master taskloop reduction(+:r10[1:12]) default(none)
  for (int i = 0; i < 64; i++)
    r10[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r11\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r11 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r11 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r11 \\+ 4" "gimple" } } */
  #pragma omp parallel master taskloop simd reduction(+:r11[1:13]) default(none)
  for (int i = 0; i < 64; i++)
    r11[1]++;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r12 \\+ 4" "gimple" } } *//* FIXME: This should be on sections instead.  */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r12 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r12 \\+ 4" "gimple" } } */
  #pragma omp parallel sections reduction(+:r12[1:14]) default(none)
  {
    r12[1]++;
    #pragma omp section
    r12[1]++;
  }
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 60\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r13 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r13\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r13 \\+ 4" "gimple" } } */
  #pragma omp target parallel reduction(+:r13[1:15]) default(none) defaultmap(none)
  r13[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 64\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r14 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r14" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r14 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r14 \\+ 4" "gimple" } } *//* FIXME.  */
  #pragma omp target parallel for reduction(+:r14[1:16]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r14[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 68\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r15 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r15 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r15 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r15 \\+ 4" "gimple" } } */
  #pragma omp target parallel for simd reduction(+:r15[1:17]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r15[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 72\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r16 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r16\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r16 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r16 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target parallel loop reduction(+:r16[1:18]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r16[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 76\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r17 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r17\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r17 \\+ 4" "gimple" } } */
  #pragma omp target teams reduction(+:r17[1:19]) default(none) defaultmap(none)
  r17[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 80\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r18 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r18\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r18 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r18 \\+ 4" "gimple" } } */
  #pragma omp target teams distribute reduction(+:r18[1:20]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r18[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 84\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r19 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r19 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r19 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r19 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r19 \\+ 4" "gimple" } } *//* FIXME.  */
  #pragma omp target teams distribute parallel for reduction(+:r19[1:21]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r19[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 88\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r20 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r20 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r20 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r20 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r20 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r20 \\+ 4" "gimple" } } */
  #pragma omp target teams distribute parallel for simd reduction(+:r20[1:22]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r20[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 92\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r21 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r21 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r21 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r21 \\+ 4" "gimple" } } */
  #pragma omp target teams distribute simd reduction(+:r21[1:23]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r21[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 96\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r22 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(r22\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r22 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r22\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r22 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r22 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target teams loop reduction(+:r22[1:24]) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r22[1]++;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:\\\*_\[0-9]* \\\[len: 100\\\]" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(firstprivate:r23 \\\[pointer assign, bias: 4\\\]\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(r23\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r23 \\+ 4" "gimple" } } */
  #pragma omp target simd reduction(+:r23[1:25]) defaultmap(none)
  for (int i = 0; i < 64; i++)
    r23[1]++;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r24 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r24 \\+ 4" "gimple" } } */
  #pragma omp taskloop simd reduction(+:r24[1:26]) default(none)
  for (int i = 0; i < 64; i++)
    r24[1]++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r25 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r25 \\+ 4" "gimple" } } */
  #pragma omp teams distribute reduction(+:r25[1:27]) default(none)
  for (int i = 0; i < 64; i++)
    r25[1]++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r26 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r26 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r26 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r26 \\+ 4" "gimple" } } *//* FIXME.  */
  #pragma omp teams distribute parallel for reduction(+:r26[1:28]) default(none)
  for (int i = 0; i < 64; i++)
    r26[1]++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r27 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r27 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r27 \\+ 4" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r27 \\+ 4" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r27 \\+ 4" "gimple" } } */
  #pragma omp teams distribute parallel for simd reduction(+:r27[1:29]) default(none)
  for (int i = 0; i < 64; i++)
    r27[1]++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r28 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r28 \\+ 4" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r28 \\+ 4" "gimple" } } */
  #pragma omp teams distribute simd reduction(+:r28[1:30]) default(none)
  for (int i = 0; i < 64; i++)
    r28[1]++;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*firstprivate\\(r29\\)" "gimple" } } *//* FIXME: Should be shared, but firstprivate is an optimization.  */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r29 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*firstprivate\\(r29\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r29 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*reduction\\(\\+:MEM\[^\n\r]*\\)r29 \\+ 4" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp teams loop reduction(+:r29[1:31]) default(none)
  for (int i = 0; i < 64; i++)
    r29[1]++;
}
