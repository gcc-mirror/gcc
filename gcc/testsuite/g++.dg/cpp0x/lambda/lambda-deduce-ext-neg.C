// Testcase for DR 975.

// { dg-options -std=c++0x }

bool b;
struct A { int fn1(); const int& fn2(); };
struct B { int fn1(); long fn2(); };

template <class T> int f (T t) {
  return [](T t){
    if (b)
      return t.fn1();
    else
      return t.fn2();		// { dg-error "inconsistent types" }
  }(t);
}

int main()
{
  f(A());			// { dg-bogus "" } int and const int& are compatible
  f(B());			// { dg-message "from here" } int and long are not
}
