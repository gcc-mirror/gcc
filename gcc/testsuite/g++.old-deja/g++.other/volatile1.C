// Test that failed lvalue-to-rvalue conversion of vf doesn't crash the
// compiler.

class f_class
{ };				// ERROR - candidates

volatile f_class
ret_v_f_class()
{
  f_class t;
  return t;
}

int main(void)
{
  volatile f_class vf;
  0 ? ret_v_f_class() : vf;	// ERROR - can't copy volatile lvalue
  return 0;
}
