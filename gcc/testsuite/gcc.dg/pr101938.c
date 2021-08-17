// { dg-do run }
// { dg-require-effective-target lp64 }
// { dg-options "-O2 -fwrapv" }

typedef long long int int64;
#define INT64CONST(x) (x##LL)
/* -9223372036854775808ULL */
#define INT64_MIN (-INT64CONST(0x7FFFFFFFFFFFFFFF) - 1)

static void __attribute__((noipa)) foo(int64 arg1, int64 arg2) {
  int64 a1 = -arg1;
  int64 a2 = (arg2 < 0) ? arg2 : -arg2;

  if (a1 > a2) {
    int64 swap = arg1;
    arg1 = arg2;
    arg2 = swap;
  }

  if (arg1 == INT64_MIN && arg2 == -1) return;

  __builtin_abort();
}

int main() {
  foo(-1, INT64_MIN);
  return 0;
}
