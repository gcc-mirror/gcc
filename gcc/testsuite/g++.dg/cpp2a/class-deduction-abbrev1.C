 // { dg-do compile { target c++20 } }

template <class T> struct A { };
template <class T> concept is_A = requires { A(T()); };

void f(auto);      // OK
void f(is_A auto); // OK
void f(A);	   // { dg-error "placeholder" }

int main()
{
  f(A<int>());
}
