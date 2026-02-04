/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */

long foo(long a, long b) __attribute__((target("arch=+randomstring"))); /* { dg-error "unexpected arch for 'target\\(\\)' attribute: bad string found '\\+randomstring'" } */
long foo(long a, long b){
  return a + (b * 2);
}
