// { dg-do compile }
// { dg-options "-g" }
// Contributed by: <schmid at snake dot iap dot physik dot tu-darmstadt dot de>
//   and Niall Douglas <s_gccbugzilla at nedprod dot com>
// PR c++/14246: ice in write_template_arg_literal while mangling boolean
//   expressions.

namespace N1 {

  template <typename T>
  struct A {
      enum { Yes = (sizeof(T) == 1) };
  };

  template<bool T>
  struct B {
      void foo(void);
  };

  template struct B< !A<int>::Yes >;

}


namespace N2 {

  template<bool> struct A {};
  A<!false> a;

}

