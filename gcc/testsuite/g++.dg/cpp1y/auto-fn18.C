// { dg-do compile { target c++14 } }

struct A
{
  virtual int f() { return 1; }	 // { dg-message "overridden" }
  virtual auto g() { return 1; } // { dg-error "11:virtual" }
};

struct B: A
{
  auto f() { return 1; }	// { dg-error "return type" }
};
