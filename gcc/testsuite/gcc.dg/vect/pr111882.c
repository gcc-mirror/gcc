/* { dg-do compile } */
/* { dg-additional-options { -fdump-tree-ifcvt-all } } */

static void __attribute__((noipa)) f(int n) {
  int i, j;
  struct S { char d[n]; int a; int b : 17; int c : 12; };
  struct S A[100][1111];
  for (i = 0; i < 100; i++) {
    asm volatile("" : : "g"(&A[0][0]) : "memory");
    for (j = 0; j < 1111; j++) A[i][j].b = 2;
  }
}
void g(void) { f(1); }

/* { dg-final { scan-tree-dump-not "Bitfield OK to lower" "ifcvt" } } */
