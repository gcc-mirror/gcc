// PR c++/115501
// { dg-do compile }

struct s{virtual void f();};
struct s1 : s{};
namespace __cxxabiv1
{
  extern "C" void __dynamic_cast(); // { dg-message "previous declaration" }
}
void diagnostic_information_impl(s const *se)
{
  dynamic_cast<s1 const *>(se);
}

// { dg-error "conflicting declaration" "" { target *-*-* } 0 }
