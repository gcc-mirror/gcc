/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */

struct nilfs_segment_usage {
  int su_flags;
} a;
enum { NILFS_SEGMENT_USAGE_ACTIVE, NILFS_SEGMENT_USAGE_DIRTY } fn1();
int b;
void fn2(int *, long long);
void fn3() {
  int c, d;
  struct nilfs_segment_usage e = a;
  fn1();
  c = e.su_flags & 1 << NILFS_SEGMENT_USAGE_DIRTY;
  d = c;
  fn2(&b, d ? -1 : 0);
}
