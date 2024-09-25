/* { dg-do run } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -fno-ext-dce" } */

extern void abort (void);

int a = 2;
unsigned b = 0x80000000;
int arr_5[2][23];
void test(int, unsigned, int);
int main() {
  test(a, b, 1);
  if (arr_5[1][0] != -2147483648)
    abort ();
  return 0;
}

#define c(a, b)                                                                \
  ({                                                                           \
    long d = a;                                                                \
    long e = b;                                                                \
    d > e ? d : e;                                                             \
  })
__attribute__((noipa))
void test(int f, unsigned g, int h) {
  for (int i = 0; i < h; i = f)
    arr_5[1][i] = h ? c(g, 7) : 0;
}

