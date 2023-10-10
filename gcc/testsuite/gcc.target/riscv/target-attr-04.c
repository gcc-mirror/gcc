/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc_zba -O2 -mabi=lp64 -mtune=rocket" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**   ...
**   # tune = sifive-7-series
**   ...
**   slli\s*a1,a1,1
**   add\s*a0,a1,a0
**   ...
*/
long foo() __attribute__((target("cpu=sifive-u74")));
long foo(long a, long b){
  return a + (b * 2);
}

/*
** bar:
**   ...
**   sh1add\s*a0,a1,a0
**   ...
*/
long bar(long a, long b){
  return a + (b * 2);
}
