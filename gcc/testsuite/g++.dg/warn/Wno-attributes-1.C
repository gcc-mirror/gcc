// { dg-do compile { target c++11 } }
// { dg-options "-Wno-attributes=bar:: -Wno-attributes=baz::qux" }

[[foo::bar]];				// { dg-warning "attribute ignored" }
[[bar::foo, foo::bar, baz::qux]];	// { dg-warning "attribute ignored" }
[[bar::foo, bar::bar, baz::qux]];	// { dg-bogus "attribute ignored" }

namespace [[foo::bar]] N {		// { dg-warning "'bar' attribute directive ignored" }
  int n;
}
namespace [[bar::foo, foo::bar, baz::qux]] O { // { dg-warning "'bar' attribute directive ignored" }
  int o;
}
namespace [[bar::foo, bar::bar, baz::qux]] P { // { dg-bogus "attribute directive ignored" }
  int p;
}

void
foo ()
{
  int i = 0;
  [[foo::bar]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[bar::foo, foo::bar, baz::qux]];	// { dg-warning "attributes at the beginning of statement are ignored" }
  [[bar::foo, bar::bar, baz::qux]];	// { dg-bogus "attributes at the beginning of statement are ignored" }
  [[foo::bar]] ++i;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[bar::foo, foo::bar, baz::qux]] ++i;	// { dg-warning "attributes at the beginning of statement are ignored" }
  [[bar::foo, bar::bar, baz::qux]] ++i;	// { dg-bogus "attributes at the beginning of statement are ignored" }
  [[foo::bar]] asm ("");		// { dg-warning "attributes ignored on 'asm' declaration" }
  [[bar::foo, foo::bar, baz::qux]] asm (""); // { dg-warning "attributes ignored on 'asm' declaration" }
  [[bar::foo, bar::bar, baz::qux]] asm (""); // { dg-bogus "attributes ignored on 'asm' declaration" }
  [[foo::bar]] using namespace N;	// { dg-warning "'bar' attribute directive ignored" }
  [[bar::foo, foo::bar, baz::qux]] using namespace O; // { dg-warning "'bar' attribute directive ignored" }
  [[bar::foo, bar::bar, baz::qux]] using namespace P; // { dg-bogus "attribute directive ignored" }
}

class S
{
  [[foo::bar]] friend int bar (S &);	// { dg-warning "attribute ignored" }
					// { dsg-message "an attribute that appertains to a friend declaration that is not a definition is ignored" "" { target *-*-* } .-1 }
  [[bar::foo, foo::bar, baz::qux]] friend int baz (S &); // { dg-warning "attribute ignored" }
					// { dsg-message "an attribute that appertains to a friend declaration that is not a definition is ignored" "" { target *-*-* } .-1 }
  [[bar::foo, bar::bar, baz::qux]] friend int qux (S &); // { dg-warning "attribute ignored" }
					// { dsg-message "an attribute that appertains to a friend declaration that is not a definition is ignored" "" { target *-*-* } .-1 }
public:
  int s;
};

int [[foo::bar]] i;			// { dg-warning "attribute ignored" }
					// { dg-message "an attribute that appertains to a type-specifier is ignored" "" { target *-*-* } .-1 }
int [[bar::foo, foo::bar, baz::qux]] j;	// { dg-warning "attribute ignored" }
					// { dg-message "an attribute that appertains to a type-specifier is ignored" "" { target *-*-* } .-1 }
int [[bar::foo, bar::bar, baz::qux]] k;	// { dg-bogus "attribute ignored" }
