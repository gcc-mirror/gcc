/* { dg-lto-options "-mcpu=v9" { target sparc*-*-* } } */
/* { dg-require-effective-target sync_char_short } */
void
_cairo_clip_path_reference () {
  int a;
  __sync_fetch_and_add(&a, 1);
}

int main(void) {
  return 0;
}
