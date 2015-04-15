// PR c++/22618

class foo
{
  typedef int memfun;  // { dg-message "private" }
};

template<foo::memfun> // { dg-error "context" }
struct fm_obj { };

template <typename T = foo::memfun> // { dg-error "context" }
struct S {};
