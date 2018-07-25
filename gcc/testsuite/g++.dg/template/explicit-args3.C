// PR c++/37177

template <class T>
struct A { };

template <class T>
void operator+(T, T);		// { dg-error "6:.void operator\\+\\(T, T\\) \\\[with T = int\\\]. must have an argument of class or enumerated type" }

int main()
{
  operator+<int>;		// { dg-error "cannot resolve" }
}
