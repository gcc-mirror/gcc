short s;

int
foo()
{
  s %= 0; /* { dg-warning "division by zero" } */
  return s > 0;
}
