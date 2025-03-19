/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32ixtheadmempair -mabi=ilp32 -mno-strict-align" } */

struct a {
  signed : 22;
};
volatile short b;
int *c;
void d(int e, struct a) {
  b;
  c = &e;
}
