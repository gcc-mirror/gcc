// { dg-do compile { target c++14 } }

template <class T> struct A { };
void f(A<auto> a) { }		// { dg-error "auto. in template argument" }
// { dg-message "in parameter declaration" "" { target c++17_down } .-1 }
int main()
{
  f(A<int>());
}
