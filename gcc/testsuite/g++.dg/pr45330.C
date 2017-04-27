// { dg-do compile }
// Search std, __cxxabiv1, and global namespaces, plus one more.
// { dg-options "--param cxx-max-namespaces-for-diagnostic-help=4" }

#define NSPACE(NAME) namespace NAME { int foo; }

namespace A
{
  int foo;			// { dg-message "A::foo" "suggested alternative" }
}

namespace B
{
  int foo;
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
  // { dg-message "maximum limit of 4 namespaces" "maximum limit" { target *-*-* } .-1 }
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } .-2 }
}
