/* { dg-do compile } */
/* { dg-options "-O2 -mbig-endian" } */

long long a;
enum { NILFS_SEGMENT_USAGE_ACTIVE, NILFS_SEGMENT_USAGE_DIRTY } b;
void nilfs_sufile_mod_counter(long long p1) {
  long c = p1;
  unsigned d = __builtin_bswap64(a);
  a = __builtin_bswap64(d + c);
}
void nilfs_sufile_do_free() {
  int e, f;
  e = __builtin_bswap32(b) & 1 << NILFS_SEGMENT_USAGE_DIRTY;
  f = e;
  nilfs_sufile_mod_counter(f ? -1 : 0);
}
