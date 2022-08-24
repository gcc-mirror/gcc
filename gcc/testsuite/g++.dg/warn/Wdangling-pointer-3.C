/* PR middle-end/104076 - bogus -Wdangling-pointer on a conditional expression
   { dg-do compile { target { c++11 } } }
   { dg-options "-Wall" } */

namespace std {

template <class T>
struct initializer_list
{
  T *array;
  __SIZE_TYPE__ nelts;

  initializer_list (const T *a, __SIZE_TYPE__ n)
    : array (a), nelts (n) { }

  initializer_list()
  : array (), nelts () { }

  T* begin () const { return array; }

  const T* end () const { return array + nelts; }
};

}

struct S1
{
  S1 (int);
  ~S1 ();
};

struct S2 { S2 (std::initializer_list<S1>); };

S2 f1();

S2 f2(bool b)
{
  return b ? f1() : S2{0};    // { dg-bogus "-Wdangling-pointer" }
}
