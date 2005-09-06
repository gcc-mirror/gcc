// { dg-do compile }
// Origin: <struppi@acm.org>

// PR c++/8591
// Template or class detection in friend declaration

namespace NS {
  template <class T1, class T2, class T3 = int, class T4 = int>
  struct C {};
}

template <class T> class X {
  friend class NS::C;	// { dg-error "template|friend" }
};

X<int> c;
