TYPE char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));
TYPE char Y[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__)));

__attribute__ ((noinline)) int
foo1(int len) {
  int i;
  TYPE int result = 0;
  TYPE short prod;

  for (i=0; i<len; i++) {
    prod = X[i] * Y[i];
    result += prod;
  }
  return result;
}