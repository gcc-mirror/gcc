class A
{ };

class B; // { dg-message "forward declaration" }

union C
{ };

union D;

void f()
{
  __builtin_is_virtual_base_of(A, B);  // { dg-error "incomplete type" }
  __builtin_is_virtual_base_of(C, D);  
}
