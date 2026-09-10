// PR c++/126653
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace std { }
struct S { };
namespace N { int foo; };

namespace GS = [: ^^:: :];	// { dg-error "splice of the global namespace" }
using namespace [: ^^GS :];	// { dg-error "declared|expected" }

using namespace [: ^^int :];	// { dg-error "expected a reflection of a namespace" }
namespace X = [: ^^void :];	// { dg-error "expected a reflection of a namespace" }

using namespace S::T;		// { dg-error ".T. is not a namespace-name" }

template<auto ns>
void
bad ()
{
  using namespace [:ns:];	// { dg-error "dependent namespaces cannot appear in a using directive" }
  using namespace ::[:ns:];	// { dg-error "expected" }
  using namespace N::[:ns:];	// { dg-error "expected" }
  using namespace [:ns:]::N;	// { dg-error "dependent namespaces cannot appear in a using directive" }
  using namespace [: ^^:: :];	// { dg-error "splice of the global namespace can only appear in a nested-name-specifier" }
  using namespace ::;		// { dg-error "expected identifier before" }

  namespace A = [:ns:];
  using namespace A::N;		// { dg-error "dependent namespaces cannot appear in a using directive" }
  using namespace A::N::M;	// { dg-error "dependent namespaces cannot appear in a using directive" }
}

template<typename T>
void
also_bad ()
{
  using namespace T::N;		// { dg-error "dependent namespaces cannot appear in a using directive" }
}

int
ok ()
{
  using namespace [: ^^:: :]::N;
  using namespace [: ^^N :];
  using namespace [: ^^::N :];
  using namespace ::N;
  using namespace [: ^^std :];
  namespace B = [: ^^N :];
  using namespace B;
  return foo;
}

template<typename>
void
also_ok ()
{
  namespace B = [: ^^N :];
  using namespace B;
  using namespace [: ^^N :];
}
template void also_ok<int> ();
