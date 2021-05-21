/* { dg-do compile } */
/* { dg-options "-fgimple" } */

__GIMPLE
void foo() {
  int t1;
  t1_1 = t1_1(); /* { dg-error "invalid call" } */
}
