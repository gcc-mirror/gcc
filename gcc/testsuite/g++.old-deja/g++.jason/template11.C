// { dg-do run  }
// Bug: initializers for static data members of templates don't get run.

template <class T> struct A {
  static T t;
};

int foo () { return 1; }

template <>
int A<int>::t = foo ();

int main ()
{
  return (A<int>::t != 1);
}
