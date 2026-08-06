// PR c++/125647
// { dg-do compile { target c++11 } }

struct A { int a; };
namespace std {
  template <class T>
  class initializer_list : A { T *d; decltype (sizeof 0) s; };	// { dg-error "definition of 'class std::initializer_list<T>' does not match '#include <initializer_list>'" }
}

void foo (std::initializer_list <int>);

void
bar ()
{
  foo ({ 1, 2, 3 });
}

// { dg-prune-output "compilation terminated" }
