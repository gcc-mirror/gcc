// { dg-do compile { target c++14 } }

struct A
{
  virtual int f() { return 1; }	 // { dg-message "overriding" }
  virtual auto g() { return 1; } // { dg-error "virtual" }
};

struct B: A
{
  auto f() { return 1; }	// { dg-error "return type" }
};
