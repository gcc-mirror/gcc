/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */


long foo(long a, long b) __attribute__((target("arch=+zbb_zba"))); /* { dg-error "extension 'zbb_zba' starts with 'z' but is unsupported standard extension" } */
long foo(long a, long b){
  return a + (b * 2);
}

long bar(long a, long b){
  return a + (b * 2);
}
