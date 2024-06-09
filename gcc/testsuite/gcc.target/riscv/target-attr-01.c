/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-options "-march=rv64gc -O2 -mabi=lp64" } */
/* { dg-final { check-function-bodies "**" "" } } */


/*
** foo:
**   ...
**   sh1add\s*a0,a1,a0
**   ...
*/


long foo() __attribute__((target("arch=rv64gc_zba")));
long foo(long a, long b){
  return a + (b * 2);
}

/*
** bar:
**   ...
**   slli\s*a1,a1,1
**   add\s*a0,a1,a0
**   ...
*/


long bar(long a, long b){
  return a + (b * 2);
}
