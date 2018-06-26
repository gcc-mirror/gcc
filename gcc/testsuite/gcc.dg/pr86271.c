/* { dg-do compile } */
/* { dg-options "-O2" } */

int main ()
{
  int i;
  foobar (i, &i); /* { dg-warning "implicit declaration" } */
}

int foobar (int a, long long b)
{
  int c;

  c = a % b;
  a = a / b;
  return a + b;
}
