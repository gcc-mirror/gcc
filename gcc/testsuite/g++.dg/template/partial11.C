// DR 115

// 14.8.1: In contexts where deduction is done and fails, or in contexts
// where deduction is not done, if a template argument list is specified
// and it, along with any default template arguments, identifies a single
// function template specialization, then the template-id is an lvalue for
// the function template specialization.

// Here, deduction is not done to resolve fn<int> because the target type
// is a template parameter, so we resolve to the second template, and then
// the call to def fails because we deduce different values of Fn for the
// two function arguments.

template <class Fn> void def(Fn fn, Fn fn2);

template <class T1, class T2> T2 fn(T1, T2);
template <class T1> int fn(T1);

int f(int,int);

int main()
{
  def(fn<int>,f);		// { dg-error "" }
}
