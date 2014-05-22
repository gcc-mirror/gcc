/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

void f() { }
int main()
{
  f[0:1:1];  // { dg-error "function type" }
}
