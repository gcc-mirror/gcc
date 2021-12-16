/* { dg-do compile } */
/* { dg-options "-mabi=ilp32 -mstrict-align -O2" } */

int unsigned_range_min, unsigned_range_max, a11___trans_tmp_1;

void a11() {
  a11___trans_tmp_1 = unsigned_range_max < unsigned_range_min;
  __builtin_memset((char *)1, 0, a11___trans_tmp_1);
}
