// Build don't link:

// Overly simplified from testcase by "B. K. Oxley" <binkley@bigfoot.com>

template<class P, class Q> struct foo {
  typedef P parent_type;
  friend parent_type; // ERROR - template parameters cannot be friends
  friend Q;           // ERROR - template parameters cannot be friends
};
