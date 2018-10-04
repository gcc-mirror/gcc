/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

__attribute__ ((target ("default")))
static unsigned foo(const char *buf, unsigned size) {
  return 1;
}

__attribute__ ((target ("avx")))
static unsigned foo(const char *buf, unsigned size) {
  return 2;
}

__attribute__ ((target ("default")))
unsigned bar() {
  char buf[4096];
  unsigned acc = 0;
  for (int i = 0; i < sizeof(buf); i++) {
    acc += foo(&buf[i], 1);
  }
  return acc;
}

__attribute__ ((target ("avx")))
unsigned bar() {
  char buf[4096];
  unsigned acc = 0;
  for (int i = 0; i < sizeof(buf); i++) {
    acc += foo(&buf[i], 1);
  }
  return acc;
}

/* { dg-final { scan-tree-dump-times "return 4096;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 8192;" 1 "optimized" } } */
