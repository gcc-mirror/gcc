// PR c++/86439
// { dg-do compile { target c++17 } }

struct NC {
  NC() = default;
  NC(NC const&) = delete;
  NC& operator=(NC const&) = delete;
};

template <int>
struct C {
  C(NC const&);
};

C(NC) -> C<0>;

NC nc;
C c(nc);
