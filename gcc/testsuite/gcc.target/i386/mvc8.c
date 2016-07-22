/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O3 -fno-inline" } */
/* { dg-final { scan-assembler-not "constprop" } } */
__attribute__((target_clones("arch=core-avx2","arch=slm","default")))
void foo (float *a, int b) {
    *a = (float)b;
}
float a;
int main() {
  int i;
  for (i = 0; i < 1024; i++)
    foo (&a, 5);
}
