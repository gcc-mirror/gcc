// 12.1: A constructor shall not be declared with a ref-qualifier.
// 12.4: A destructor shall not be declared with a ref-qualifier.

// { dg-require-effective-target c++11 }

struct A {
  A() & = default;		// { dg-error "constructor" }
  ~A() & = default;		// { dg-error "destructor" }
};

int main()
{
  A a;
}
