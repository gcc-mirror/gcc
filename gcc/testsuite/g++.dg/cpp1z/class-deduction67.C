// Deduction from inherited constructors isn't supported yet, but we shouldn't
// crash.  It may well be supported in C++23.

//{ dg-do compile { target c++17 } }

template <class T> struct A
{
  A(T);
};

template <class T> struct B: A<T>
{
  using A<T>::A;
};

int main()
{
  B b = 42;			// { dg-line init }
  // { dg-prune-output "no matching function" }
  // { dg-error "class template argument deduction" "" { target *-*-* } init }
}
