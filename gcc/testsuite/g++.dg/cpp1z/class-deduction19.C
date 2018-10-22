// PR c++/77912
// { dg-do compile { target c++17 } }

template<class T> struct S{S(T){}}; 

//error: invalid use of template type parameter 'S'
template<class T> auto f(T t){return S(t);}

int main()
{
  //fails
  f(42);

  //fails
  //error: invalid use of template type parameter 'S'
  [](auto a){return S(a);}(42); 

  //works
  [](int a){return S(a);}(42);
}
