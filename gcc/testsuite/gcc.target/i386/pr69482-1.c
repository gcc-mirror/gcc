/* { dg-do compile } */
/* { dg-options "-O3 -fno-stack-protector" } */

static inline void memset_s(void* s, int n) {
  volatile unsigned char * p = s;
  for(int i = 0; i < n; ++i) {
    p[i] = 0;
  }
}

void test() {
  unsigned char x[4];
  memset_s(x, sizeof x);
}

/* { dg-final { scan-assembler-times "mov" 4 } } */
