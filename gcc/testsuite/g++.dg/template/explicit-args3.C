// PR c++/37177

template <class T>
struct A { };

template <class T>
void operator+(T, T);		// { dg-error "class or enum" }

int main()
{
  operator+<int>;		// { dg-error "cannot resolve" }
}
