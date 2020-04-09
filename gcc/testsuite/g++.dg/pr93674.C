// { dg-do compile }
// { dg-options "-O3 -std=c++14 -fstrict-enums -pedantic -fdump-tree-optimized" }
enum some_enum { x = 1000 };
void sink(some_enum);

int __attribute__((noinline)) func() {
  int sum = 0;
  for (int i = 0; i < 3; ++i) {
      for (int j = 3; j >= 0; --j) {
          sink((some_enum)(i + j));
      }
  }
  return sum;
}

// { dg-final { scan-tree-dump-not "some_enum ivtmp" "optimized" } }
