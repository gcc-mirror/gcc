// In a .* expression whose object expression is an rvalue, the program is
// ill-formed if the second operand is a pointer to member function with
// ref-qualifier &. In a .* expression whose object expression is an
// lvalue, the program is ill-formed if the second operand is a pointer to
// member function with ref-qualifier &&.

// { dg-require-effective-target c++11 }

struct A {
  void f() &;
  void g() &&;
  void h();
};

void one()
{
  A a;

  void (A::*p)() & = &A::f;
  (a.*p)();
  (A().*p)();			// { dg-error "" }

  p = &A::g;			// { dg-error "" }
  p = &A::h;			// { dg-error "" }

  void (A::*p2)() && = &A::g;
  (A().*p2)();
  (a.*p2)();			// { dg-error "" }
  p2 = &A::f;			// { dg-error "" }
  p2 = &A::h;			// { dg-error "" }

  void (A::*p3)() = &A::h;
  (a.*p3)();
  (A().*p3)();
  p3 = &A::f;			// { dg-error "" }
  p3 = &A::g;			// { dg-error "" }
}

template <class T>
struct B {
  void f() &;
  void g() &&;
  void h();
};

template <class T>
void two()
{
  B<T> a;

  void (B<T>::*p)() & = &B<T>::f;
  (a.*p)();
  (B<T>().*p)();		// { dg-error "" }

  p = &B<T>::g;			// { dg-error "" }
  p = &B<T>::h;			// { dg-error "" }

  void (B<T>::*p2)() && = &B<T>::g;
  (B<T>().*p2)();
  (a.*p2)();			// { dg-error "" }
  p2 = &B<T>::f;		// { dg-error "" }
  p2 = &B<T>::h;		// { dg-error "" }

  void (B<T>::*p3)() = &B<T>::h;
  (a.*p3)();
  (B<T>().*p3)();
  p3 = &B<T>::f;		// { dg-error "" }
  p3 = &B<T>::g;		// { dg-error "" }
}

int main()
{
  one();
  two<int>();
}
