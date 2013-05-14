// typedef test
// { dg-require-effective-target c++11 }

typedef void F() &;

F f;				// { dg-error "" }
F* p;				// { dg-error "" }
extern F& r;			// { dg-error "" }

struct A {
  F f;
};

int main()
{
  A a;
  a.f();
  A().f();			// { dg-error "" }
}
