/* PR rtl-optimization/57130 */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ira" } */
/* { dg-final { scan-rtl-dump-not "REG_EQUIV\[^\n\]*mem\[^\n\]*\"ip\"" "ira" } } */

char ip[10];
int total;

void foo() {
  int t;

  t = ip[2];
  total = t & 0x3;
}
