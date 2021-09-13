/* PR middle-end/99928 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int l00, l01, l02, l03, l04, l05, l06, l07;
int l10, l11, l12, l13, l14, l15, l16, l17, l18;

void
foo (void)
{
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l00\\)" "gimple" } } *//* FIXME.  */
  #pragma omp distribute parallel for lastprivate (l00) default(none)
  for (int i = 0; i < 64; i++)
    l00 = i;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l01\\)" "gimple" } } */
  #pragma omp distribute parallel for simd lastprivate (l01) default(none)
  for (int i = 0; i < 64; i++)
    l01 = i;
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l02\\)" "gimple" } } */
  #pragma omp distribute simd lastprivate (l02)
  for (int i = 0; i < 64; i++)
    l02 = i;
}

void
bar (void)
{
  int j00, j01, j02, j03;
  int l08 = 0, l09 = 0, l19 = 0, l20 = 0, l21 = 0, l22 = 0;
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l03\\)" "gimple" } } */
  #pragma omp for simd lastprivate (l03)
  for (int i = 0; i < 64; i++)
    l03 = i;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l04\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l04\\)" "gimple" } } */
  #pragma omp master taskloop lastprivate (l04) default(none)
  for (int i = 0; i < 64; i++)
    l04 = i;
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l05\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l05\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l05\\)" "gimple" } } */
  #pragma omp master taskloop simd lastprivate (l05) default(none)
  for (int i = 0; i < 64; i++)
    l05 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l06\\)" "gimple" } } *//* FIXME.  */
  #pragma omp parallel for lastprivate (l06) default(none)
  for (int i = 0; i < 64; i++)
    l06 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l07\\)" "gimple" } } */
  #pragma omp parallel for simd lastprivate (l07) default(none)
  for (int i = 0; i < 64; i++)
    l07 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j00\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j00\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp parallel loop lastprivate (j00) default(none)
  for (j00 = 0; j00 < 64; j00++)
    ;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l08\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l08\\)" "gimple" } } */
  #pragma omp parallel master taskloop lastprivate (l08) default(none)
  for (int i = 0; i < 64; i++)
    l08 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(l09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp master\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l09\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l09\\)" "gimple" } } */
  #pragma omp parallel master taskloop simd lastprivate (l09) default(none)
  for (int i = 0; i < 64; i++)
    l09 = i;
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l10\\)" "gimple" } } *//* FIXME: This should be on sections instead.  */
  /* { dg-final { scan-tree-dump-not "omp sections\[^\n\r]*lastprivate\\(l10\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump-not "omp section \[^\n\r]*lastprivate\\(l10\\)" "gimple" } } */
  #pragma omp parallel sections lastprivate (l10) default(none)
  {
    l10 = 1;
    #pragma omp section
    l10 = 2;
  }
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l11" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l11\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l11\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target parallel for lastprivate (l11) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l11 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l12" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l12\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l12\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l12\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l12\\)" "gimple" } } */
  #pragma omp target parallel for simd lastprivate (l12) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l12 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j01" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j01\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j01\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j01\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target parallel loop lastprivate (j01) default(none) defaultmap(none)
  for (j01 = 0; j01 < 64; j01++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l13" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l13\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l13\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l13\\)" "gimple" } } */
  #pragma omp target teams distribute lastprivate (l13) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l13 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l14" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l14\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l14\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l14\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l14\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l14\\)" "gimple" } } *//* FIXME.  */
  #pragma omp target teams distribute parallel for lastprivate (l14) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l14 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l15" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l15\\)" "gimple" } } */
  #pragma omp target teams distribute parallel for simd lastprivate (l15) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l15 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l16" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l16\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l16\\)" "gimple" } } */
  #pragma omp target teams distribute simd lastprivate (l16) default(none) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l16 = i;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:j02" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(j02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j02\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j02\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j02\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp target teams loop lastprivate (j02) default(none) defaultmap(none)
  for (j02 = 0; j02 < 64; j02++)
    ;
  /* { dg-final { scan-tree-dump "omp target\[^\n\r]*map\\(tofrom:l17" "gimple" } } */
  /* { dg-final { scan-tree-dump-not "omp target\[^\n\r]*firstprivate\\(l17\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l17\\)" "gimple" } } */
  #pragma omp target simd lastprivate (l17) defaultmap(none)
  for (int i = 0; i < 64; i++)
    l17 = i;
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*shared\\(l18\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp taskloop\[^\n\r]*lastprivate\\(l18\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l18\\)" "gimple" } } */
  #pragma omp taskloop simd lastprivate (l18) default(none)
  for (int i = 0; i < 64; i++)
    l18 = i;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l19\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l19\\)" "gimple" } } */
  #pragma omp teams distribute lastprivate (l19) default(none)
  for (int i = 0; i < 64; i++)
    l19 = i;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l20\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l20\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l20\\)" "gimple" } } *//* FIXME.  */
 #pragma omp teams distribute parallel for lastprivate (l20) default(none)
  for (int i = 0; i < 64; i++)
    l20 = i;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } *//* FIXME: This should be on for instead.  */
  /* { dg-final { scan-tree-dump-not "omp for\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } *//* FIXME.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l21\\)" "gimple" } } */
 #pragma omp teams distribute parallel for simd lastprivate (l21) default(none)
  for (int i = 0; i < 64; i++)
    l21 = i;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(l22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(l22\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(l22\\)" "gimple" } } */
  #pragma omp teams distribute simd lastprivate (l22) default(none)
  for (int i = 0; i < 64; i++)
    l22 = i;
  /* { dg-final { scan-tree-dump "omp teams\[^\n\r]*shared\\(j03\\)" "gimple" } } */
  /* { dg-final { scan-tree-dump "omp distribute\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp parallel\[^\n\r]*shared\\(j03\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp for\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  /* { dg-final { scan-tree-dump "omp simd\[^\n\r]*lastprivate\\(j03\\)" "gimple" } } *//* NOTE: This is implementation detail.  */
  #pragma omp teams loop lastprivate (j03) default(none)
  for (j03 = 0; j03 < 64; j03++)
    ;
}
