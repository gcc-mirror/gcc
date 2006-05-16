// PR c++/22618

class foo
{
  typedef int memfun;  // { dg-error "private" }
};

template<foo::memfun>
struct fm_obj { }; // { dg-error "context" } 

template <typename T = foo::memfun> // { dg-error "context" }
struct S {};
