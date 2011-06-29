// PR c++/35255, DR 115
// { dg-do link }

// 14.8.1: In contexts where deduction is done and fails, or in contexts
// where deduction is not done, if a template argument list is specified
// and it, along with any default template arguments, identifies a single
// function template specialization, then the template-id is an lvalue for
// the function template specialization.

template <class Fn> void def(Fn fn) {}

template <class T1, class T2> T2 fn(T1, T2);
template <class T1> int fn(T1) { }

int main()
{
  def(fn<int>);
}
