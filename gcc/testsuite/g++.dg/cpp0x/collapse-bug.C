// { dg-do compile { target c++11 } }
template<typename T, typename U> struct same_type;
template<typename T> struct same_type<T, T> {};

template <typename T>
struct S
{
  typedef T const (&type)();
};

void f()
{
  // initial implementation didn't ignore const qualifier on
  // reference, resulting in a typedef of 'const int& (&)()'
  same_type<S<int &>::type, int&(&)()>();
}
