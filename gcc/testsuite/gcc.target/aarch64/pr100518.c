/* { dg-do compile } */
/* { dg-require-effective-target aarch64_mabi_ilp32 } */
/* { dg-options "-mabi=ilp32 -Wno-deprecated -mstrict-align -O2" } */

int unsigned_range_min, unsigned_range_max, a11___trans_tmp_1;

void a11() {
  a11___trans_tmp_1 = unsigned_range_max < unsigned_range_min;
  __builtin_memset((char *)1, 0, a11___trans_tmp_1); /* { dg-warning "writing 1 byte into a region of size 0" } */
}
