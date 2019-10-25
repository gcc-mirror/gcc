// { dg-do compile { target concepts } }

template<typename T, typename U = T> concept C3 = true;
template<class T> struct s1
{
  template <C3<T> U> void f() { }
};
