/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */

long foo() __attribute__((target("arch=rv64gc_zba;arch=rv64gc_zba"))); /* { dg-error "arch appears more than once" } */
long foo(long a, long b){
  return a + (b * 2);
}
