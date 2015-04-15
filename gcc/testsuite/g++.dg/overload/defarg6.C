class A
{
  int i;
  A(int i): i(i) {}		// { dg-message "private" }
};

void f (A = 1) { }		// { dg-error "context" }
