// { dg-do assemble  }

// Overly simplified from testcase by "B. K. Oxley" <binkley@bigfoot.com>

template<class P, class Q> struct foo {
  typedef P parent_type;
  friend parent_type; // { dg-error "" } template parameters cannot be friends
  friend Q;           // { dg-error "" } template parameters cannot be friends
};
