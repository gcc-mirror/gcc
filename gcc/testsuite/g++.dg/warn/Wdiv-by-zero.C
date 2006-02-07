// test that division by zero warnings are enabled by default
int breakme()
{
  int x = 0;
  x /= 0;          // { dg-warning "division by" }
  return x;
}
