class A
{ };

class B;

union C
{ };

union D;

void f()
{
  __is_base_of(A, B);  // { dg-error "incomplete type" }
  __is_base_of(C, D);  
}
