// { dg-do compile { target c++11 } }
// Search std, __cxxabiv1, and global namespaces, plus two more,
// breadth first

// { dg-options "--param cxx-max-namespaces-for-diagnostic-help=5" }

// ::, std and __cxxabiv1

namespace A
{
  int foo;			// { dg-message "A::foo" "suggested alternative" }
  namespace A0
  {
    int foo; // not me
  }
}

namespace B
{
  inline namespace I
  {
    int foo;			// { dg-message "B::I::foo" "suggested alternative" }
  }
}

namespace C
{
  int foo;
}

namespace D
{
  int foo;
}

namespace E
{
  int foo;
}

int bar()
{
  return foo;			// { dg-error "was not declared" }
  // { dg-message "maximum limit of 5 namespaces" "maximum limit" { target *-*-* } .-1 }
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } .-2 }
}
