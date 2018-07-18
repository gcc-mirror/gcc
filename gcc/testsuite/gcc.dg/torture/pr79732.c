/* { dg-do link } */
/* { dg-require-alias "" } */

int bar () __attribute__ ((alias ("foo")));
void foo () { }
int main () { return bar(); }
