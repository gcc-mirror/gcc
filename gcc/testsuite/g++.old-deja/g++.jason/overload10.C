// PRMS Id: 4257 (second bug)
// Bug: g++ fails to recognize multiple previous instantiations of a function
// template.
// Build don't link:

template <class T>
class A {
  int i;

  friend int foo (A<T>&);
};

template <class T> int foo (A<T>& a) { return a.i; }

A<int> a;
A<char> dummy;

void bar ()
{
  foo (a);			// gets bogus error - two foo(A<int>&)'s
}
