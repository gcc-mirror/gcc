/* { dg-do link } */
/* { dg-additional-options "-std=gnu17" } */
/* { dg-require-alias "" } */

int bar () __attribute__ ((alias ("foo")));
void foo () { }
int main () { return bar(); }
