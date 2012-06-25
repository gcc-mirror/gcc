// PR c++/53594
// { dg-do compile }
// { dg-options "-std=c++11 -Wuninitialized" }

struct A
{
  const int a = 6;	// { dg-bogus "non-static const member" }
  static int b;
  int &c = b;		// { dg-bogus "non-static reference" }
};

struct B
{
  const int d;		// { dg-warning "non-static const member" }
  int &e;		// { dg-warning "non-static reference" }
  int f = 7;
};
