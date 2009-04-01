// PR c++/4926
// { dg-do compile }

template <unsigned> struct X { typedef int Type; };
template <typename T> struct Y { char array[1]; };
 
template<typename T> Y<T> P(T);  // acts as "Y<typeof(T)>"
 
struct F { int operator()() const; };
 
template <typename T>
typename X<sizeof(P(  T()()  ).array)>::Type  foo();
 
void
bar () 
{ 
  foo<F>();
}
