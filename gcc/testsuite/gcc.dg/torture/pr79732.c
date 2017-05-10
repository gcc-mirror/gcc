/* { dg-do link } */

int bar () __attribute__ ((alias ("foo")));
void foo () { }
int main () { return bar(); }
