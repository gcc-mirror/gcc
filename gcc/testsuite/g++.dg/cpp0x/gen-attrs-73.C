// { dg-do compile { target c++11 } }
// Test attribute deprecated on :: with class, enum, and namespace.

struct [[deprecated]] S { static void fn(); static const int s = 0; };
union [[deprecated]] U { static void fn(); static const int u = 0; };
enum [[deprecated]] E { X };
enum class [[deprecated]] SE { Y };
namespace [[deprecated]] N { struct S { }; }

void
g ()
{
  S::fn(); // { dg-warning "deprecated" }
  (void) S::s; // { dg-warning "deprecated" }
  U::fn(); // { dg-warning "deprecated" } 
  (void) U::u; // { dg-warning "deprecated" }
  (void) E::X; // { dg-warning "deprecated" }
  (void) SE::Y; // { dg-warning "deprecated" }
  N::S s; // { dg-warning "deprecated" }
}
