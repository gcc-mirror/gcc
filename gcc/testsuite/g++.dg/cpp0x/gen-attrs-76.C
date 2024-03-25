// { dg-do compile { target c++11 } }
// { dg-options "-Wno-attributes" }

namespace N {}
namespace O { typedef int T; };
namespace P {}

void
foo ()
{
  [[]] asm ("");
  [[]] __extension__ asm ("");			// { dg-error "expected" }
  __extension__ [[]] asm ("");
  [[]] namespace M = ::N;			// { dg-error "expected" }
  [[]] using namespace N;			// { dg-bogus "expected" }
  using namespace P [[]];			// { dg-error "expected" }
  [[]] using O::T;				// { dg-error "expected" }
  [[]] __label__ foo;				// { dg-error "expected" }
  [[]] static_assert (true, "");		// { dg-error "expected" }
}

void
bar ()
{
  [[gnu::unused]] asm ("");
  [[gnu::unused]] __extension__ asm ("");	// { dg-error "expected" }
  __extension__ [[gnu::unused]] asm ("");
  [[gnu::unused]] namespace M = ::N;		// { dg-error "expected" }
  [[gnu::unused]] using namespace N;		// { dg-bogus "expected" }
  using namespace P [[gnu::unused]];		// { dg-error "expected" }
  [[gnu::unused]] using O::T;			// { dg-error "expected" }
  [[gnu::unused]] __label__ foo;		// { dg-error "expected" }
  [[gnu::unused]] static_assert (true, "");	// { dg-error "expected" }
}
