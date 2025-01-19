/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */

long foo(long a, long b) __attribute__((target("cpu=sifive-u74;cpu=sifive-u74"))); /* { dg-error "cpu appears more than once" } */
long foo(long a, long b){
  return a + (b * 2);
}
