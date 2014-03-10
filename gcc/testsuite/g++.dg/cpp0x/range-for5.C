// Test for errors in range-based for loops

// { dg-do compile { target c++11 } }

struct container
{
};

int *begin(const container &c)
{
  return 0;
}

int end(const container &c) //Ops! wrong type
{
  return 0;
}


struct Implicit
{
  Implicit(int x)
  {}
};
struct Explicit
{
  explicit Explicit(int x)
  {}
};

void test1()
{
  container c;
  for (int x : c) // { dg-error "inconsistent|conversion" }
    ;

  int a[2] = {1,2};
  for (Implicit x : a)
    ;
  for (Explicit x : a) // { dg-error "conversion" }
    ;
  for (const Implicit &x : a)
    ;
  for (Implicit &&x : a)
    ;

  //Check the correct scopes
  int i;
  for (int i : a)		// { dg-message "previously declared" }
  {
    int i;			// { dg-error "redeclaration" }
  }
}
