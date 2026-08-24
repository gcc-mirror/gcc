/* PR ipa/127023 */
/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp" } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("default", "fma4"), noinline))
static void e(int f) {
  if (!f)
#pragma omp parallel
    for (;;)
      ;
}
__attribute__((target_clones("default", "fma4")))
void g(void) {
  e(1);
}
