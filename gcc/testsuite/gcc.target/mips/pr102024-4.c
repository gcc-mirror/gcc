// { dg-do compile }
// { dg-options "-mabi=64 -mhard-float -ffat-lto-objects" }

struct __attribute__((aligned(16))) test {
  int x[0];
  double b;
  int f[];
};

void check(struct test) {} // { dg-message "the ABI for passing a value containing zero-width fields before an adjacent 64-bit floating-point field was changed in GCC 12.1" }
