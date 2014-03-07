// PR c++/50084
// { dg-do compile { target c++11 } }
// { dg-options "-fno-inline" }

template<typename>   struct remove_reference;
template<typename T> struct remove_reference<T&> { typedef T type; };

template <class T> void f(T) { }

void g()
{
  struct { } * v = 0;

  typedef remove_reference<decltype(*v)>::type at;

  // The typedef should't assign the name "at" to the struct.
  // { dg-final { scan-assembler "_Z1fIZ1gvEUt_EvT_" } }
  f(at());
}
