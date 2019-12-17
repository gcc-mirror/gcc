/* PR c++/61339 - add mismatch between struct and class
   Test to verify that -Wmismatched-tags is issued for declarations
   of the same class using different class-ids.
   { dg-do compile }
   { dg-options "-Wmismatched-tags" } */

namespace Classes
{
class A;
class A;

struct B;
struct B;

union C;
union C;

struct D;                   // { dg-warning "Classes::D' declared with a mismatched class-key 'struct'" }
class D { };                // { dg-message "Classes::D' defined as 'class' here" }

class E;                    // { dg-warning "Classes::E' declared with a mismatched class-key 'class'" }
struct E { };               // { dg-message "Classes::E' defined as 'struct' here" }

class D;
struct E;

class D;
struct E;

struct D;                   // { dg-warning "Classes::D' declared with a mismatched class-key" }

class E;                    // { dg-warning "Classes::E' declared with a mismatched class-key" }

class F;                    // { dg-message "Classes::F' first declared as 'class' here" }
class F;

struct G { };               // { dg-message "Classes::G' defined as 'struct' here" }
}   // namespace Classes


namespace Classes
{
class A;
struct B;
union C;
class D;
struct E;

struct F;                   // { dg-warning "Classes::F' declared with a mismatched class-key" }

struct G;
}

// Verify that the correct hint is provided, one to remove the class-key
// when it's redundant, and one to (only) replace it with the correct one
// when it's needed to disambiguate the reference to the class type.
namespace RemoveOrReplace
{
struct Func;
class Func;                 // { dg-warning "RemoveOrReplace::Func' declared with a mismatched class-key 'class'" }
                            // { dg-message "replace the class-key with 'struct'" "hint to remove" { target *-*-* } .-1 }

void Func ();

class Func;                 // { dg-warning "RemoveOrReplace::Func' declared with a mismatched class-key 'class'" }
                            // { dg-message "replace the class-key with 'struct'" "hint to replace" { target *-*-* } .-1 }

class Var;
struct Var;                  // { dg-warning "RemoveOrReplace::Var' declared with a mismatched class-key 'struct'" }
                            // { dg-message "replace the class-key with 'class'" "hint to remove" { target *-*-* } .-1 }
void f (struct Var*);       // { dg-warning "RemoveOrReplace::Var' declared with a mismatched class-key 'struct'" }
                            // { dg-message "remove the class-key or replace it with 'class'" "hint to remove" { target *-*-* } .-1 }

int Var;

struct Var;                  // { dg-warning "RemoveOrReplace::Var' declared with a mismatched class-key 'struct'" }
                            // { dg-message "replace the class-key with 'class'" "hint to replace" { target *-*-* } .-1 }
}

namespace GlobalObjects
{
class A;                    // { dg-message "'GlobalObjects::A' first declared as 'class' here" }
struct B;                   // { dg-message "'GlobalObjects::B' first declared as 'struct' here" }
class C { };                // { dg-message "'GlobalObjects::C' defined as 'class' here" }

extern A a0;
extern class A a1;
extern class A a2;

extern B b0;
extern struct B b1;
extern struct B b2;

extern struct A a3;         // { dg-warning "GlobalObjects::A' declared with a mismatched class-key" }
extern class A a4;

extern class B b3;          // { dg-warning "GlobalObjects::B' declared with a mismatched class-key" }
extern struct B b4;

extern struct C c[];        // { dg-warning "GlobalObjects::C' declared with a mismatched class-key" }
                            // { dg-message "remove the class-key or replace it with 'class'" "hint to remove" { target *-*-* } .-1 }

extern char
arr[sizeof (struct C)];     // { dg-warning "GlobalObjects::C' declared with a mismatched class-key" }
                            // { dg-message "remove the class-key or replace it with 'class'" "hint to remove" { target *-*-* } .-1 }
}   // namespace GlobalObjects


namespace LocalObjects
{
class A;                    // { dg-message "LocalObjects::A' first declared as 'class' here" }
struct B;                   // { dg-message "LocalObjects::B' first declared as 'struct' here" }

void f (A*, B&)
{
  class A *a1;
  class A *a2;

  struct B *b1;
  struct B *b2;

  struct A *a3;             // { dg-warning "LocalObjects::A' declared with a mismatched class-key" }
  class A *a4;

  class B *b3;              // { dg-warning "LocalObjects::B' declared with a mismatched class-key" }
  struct B *b4;
}

void g (struct A*);         // { dg-warning "LocalObjects::A' declared with a mismatched class-key" }

}   // namespace LocalObjects


namespace MemberClasses
{
struct A { struct B; };
struct C { struct D; struct D; struct D { }; };
struct E { class F; class F { }; class F; };

struct G {
  struct H;                 // { dg-message "MemberClasses::G::H' first declared as 'struct' here" }
  class H;                  // { dg-warning "MemberClasses::G::H' declared with a mismatched class-key" }
  class I { };              // { dg-message "MemberClasses::G::I' defined as 'class' here" }
  struct I;                 // { dg-warning "MemberClasses::G::I' declared with a mismatched class-key" }
};
}   // namespace MemberClasses


namespace DataMembers
{
struct A { struct B *p; };
struct C { struct D *p; struct D *q; struct D { } d; };
struct E { class F &r; class F { } f; class F *p; };

class G;                    // { dg-message "DataMembers::G' first declared as 'class' here" }
struct H;                   // { dg-message "DataMembers::H' first declared as 'struct' here" }

struct I {
  struct G *p0;             // { dg-warning "DataMembers::G' declared with a mismatched class-key" }
  class G *p1;

  struct H &r0;
  class H &r1;              // { dg-warning "DataMembers::H' declared with a mismatched class-key" }

  class J { };              // { dg-message "DataMembers::I::J' defined as 'class' here" }
  struct K { };             // { dg-message "DataMembers::I::K' defined as 'struct' here" }

  class J j0;
  class K k0;               // { dg-warning "DataMembers::I::K' declared with a mismatched class-key" }

  struct J j1;              // { dg-warning "DataMembers::I::J' declared with a mismatched class-key" }
  struct K k1;
};
}   // namespace DataMembers


namespace Templates
{
template <int> class A;
template <int> class A;

template <int> struct B;
template <int> struct B;

template <int> union C;
template <int> union C;

template <int> struct D;    // { dg-warning "Templates::D\[^\n\r]*' declared with a mismatched class-key" }
template <int>
class D                     // { dg-message "Templates::D\[^\n\r]*' defined as 'class' here" }
{ public: D (); };

template <int> class E;     // { dg-warning "Templates::E\[^\n\r]*' declared with a mismatched class-key" }
template <int>
struct E                    // { dg-message "Templates::E\[^\n\r]*' defined as 'struct' here" }
{ int i; };

template <int> class D;
template <int> struct E;

template <int>
struct D;                   // { dg-warning "Templates::D\[^\n\r]*' declared with a mismatched class-key" }
                            // { dg-message "replace the class-key with 'class'" "hint" { target *-*-* } .-1 }
}   // namespace Templates


namespace ExplicitSpecializations
{
template <int> class A;
template <> class A<0>;
template <> struct A<1>;
template <> struct A<1> { };

template <int> struct B;
template <> struct B<0>;
template <> class B<1>;
template <> class B<2> { public: B (); };

template <int> union C;
template <> union C<0>;

template <int> class D;
template <> class D<0>;     // { dg-warning "ExplicitSpecializations::D\[^\n\r]*' declared with a mismatched class-key " }
template <>
struct D<0> { };            // { dg-message "ExplicitSpecializations::D\[^\n\r]*' defined as 'struct' here" }

template <int> struct E;
template <> struct E<0>;    // { dg-warning "ExplicitSpecializations::E\[^\n\r]*' declared with a mismatched class-key" }
template <>
class E<0> { };             // { dg-message "ExplicitSpecializations::E\[^\n\r]*' defined as 'class' here" }

template <int> struct F;
template <> class F<0> { }; // { dg-message "ExplicitSpecializations::F\[^\n\r]*' defined as 'class' here" }

template <>
struct F<0>;                // { dg-warning "ExplicitSpecializations::F\[^\n\r]*' declared with a mismatched class-key" }
}   // namespace ExplicitSpecializations


namespace PartialSpecializations
{
template <class> class A;
template <class T> struct A<const T>;
template <class T> struct A<volatile T>;

template <class> struct B;
template <class T> class B<const T>;
template <class T> class B<volatile T>;

template <class> class C { };
template <class T> struct C<const T> { };
template <class T> struct C<volatile T> { };

template <class> struct D { };
template <class T> class D<const T> { };
template <class T> class D<volatile T> { };

template <class> class E;
template <class T>
struct E<const T>;          // { dg-message "PartialSpecializations::E<const T>' first declared as 'struct' here" }

template <class T>
class E<const T>;           // { dg-warning "PartialSpecializations::E<const T>' declared with a mismatched class-key" }

template <class> class F;
template <class T>
class F<const T>;           // { dg-message "PartialSpecializations::F<const T>' first declared as 'class' here" }
template <class T>
struct F<const T>;          // { dg-warning "PartialSpecializations::F<const T>' declared with a mismatched class-key" }
}   // namespace PartialSpecializations


namespace Classes
{
struct G;

class G;                    // { dg-warning "Classes::G' declared with a mismatched class-key 'class'" }
}
