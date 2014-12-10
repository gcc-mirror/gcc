// PR c++/53594
// { dg-do compile { target c++11 } }
// { dg-options "-Wuninitialized" }

struct A
{
  const int a = 6;	// { dg-bogus "non-static const member" }
  static int b;
  int &c = b;		// { dg-bogus "non-static reference" }
};

struct B
{
  virtual void g();
  const int d;		// { dg-warning "non-static const member" }
  int &e;		// { dg-warning "non-static reference" }
  int f = 7;
};
