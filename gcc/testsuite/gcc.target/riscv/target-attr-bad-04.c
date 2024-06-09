/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */


long foo() __attribute__((target(123))); /* { dg-error "argument not a string" } */
long foo(long a, long b){
  return a + (b * 2);
}

long bar(long a, long b){
  return a + (b * 2);
}
