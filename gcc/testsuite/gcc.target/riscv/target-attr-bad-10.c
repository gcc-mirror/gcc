/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */

long foo() __attribute__((target("tune=sifive-u74;tune=sifive-u74"))); /* { dg-error "tune appears more than once" } */
long foo(long a, long b){
  return a + (b * 2);
}
