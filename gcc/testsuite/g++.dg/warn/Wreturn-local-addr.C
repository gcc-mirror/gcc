// { dg-do assemble  }
// { dg-options "-Werror=return-local-addr" }
// { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 }

int& bad1()
{
  int x = 0;		// { dg-error "reference to local variable" }
  return x;
}

int* bad2()
{
  int x = 0;		// { dg-error "address of local variable" }
  return &x;
}

const int& bad4()
{
  return int();		// { dg-error "returning reference to temporary" }
}
