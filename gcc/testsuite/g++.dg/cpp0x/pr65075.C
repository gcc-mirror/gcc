// PR c++/65075
// { dg-do compile { target c++11 } }

typedef void (*E) ();
template <class T>
constexpr E
bar (bool a)
{
  return a ? []() {} : []() {};
}

void
foo ()
{
  (bar<int> (false)) ();
  (bar<int> (true)) ();
}
