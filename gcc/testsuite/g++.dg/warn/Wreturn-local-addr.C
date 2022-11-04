// { dg-do assemble  }
// { dg-options "-Werror=return-local-addr" }
// { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 }

int& bad1()
{
  int x = 0;
  return x;		// { dg-error "reference to local variable|cannot bind non-const lvalue reference" }
}

int* bad2()
{
  int x = 0;
  return &x;		// { dg-error "address of local variable" }
}

const int& bad4()
{
  return int();		// { dg-error "returning reference to temporary" }
}
