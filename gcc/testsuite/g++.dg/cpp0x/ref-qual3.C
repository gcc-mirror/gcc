// An explicitly defaulted function can have a ref-qualifier.

// { dg-require-effective-target c++11 }

struct A {
  A& operator=(const A&) & = default;
};

template<class T>
struct B {
  B& operator=(const B&) & = default;
};

template<class T>
void f()
{
  B<T> b;
  b = B<T>();
  B<T>() = b;			// { dg-error "" }
}

int main()
{
  A a;
  a = A();
  A() = a;			// { dg-error "" }

  f<int>();
}
