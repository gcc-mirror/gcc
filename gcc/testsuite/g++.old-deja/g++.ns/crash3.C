// { dg-do assemble  }

namespace N {
  template <class T> struct S;
}

void f()
{
  N::S(); // { dg-error "" } invalid use of template
}
