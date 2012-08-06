/* { dg-lto-do run } */
/* { dg-skip-if "non-IEEE math" { rx-*-* } { "*" } { "" } } */

double a(double) __attribute__ ((optimize(1), used));
double a(double r) 
{ 
  return r;
}
int main () { return 0; }
