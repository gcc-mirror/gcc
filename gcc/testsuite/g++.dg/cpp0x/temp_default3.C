// { dg-do compile { target c++11 } }

template<typename T, typename U = typename T::value_type>
void f(T);

void f(...);

struct X {
  typedef int value_type;
};

void g()
{
  f(X()); // okay
  f(17); // okay?
}
