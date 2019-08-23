// PR c++/79817 - attribute deprecated on namespace.
// { dg-do compile { target c++11 } }

namespace [[deprecated]] ns1 { int i; }
namespace [[deprecated("foo")]] ns2 { int i; }
namespace __attribute__((deprecated)) ns3 { int i; }
namespace __attribute__((deprecated("foo"))) ns4 { int i; }

namespace [[deprecated]] ns6
{
  enum E { X };
  void fn();
}

namespace [[deprecated]] ns7
{
  namespace ns8 {
    int x;
    struct Z { };
  }
  struct S { };
}

namespace N1
{
  namespace N2
  {
    namespace [[deprecated]] N3
    {
      namespace N4 { int x; }
    }
  }
}

void
f ()
{
  ns1::i = 0; // { dg-warning ".ns1. is deprecated" }
  ns2::i = 0; // { dg-warning ".ns2. is deprecated: foo" }
  ns3::i = 0; // { dg-warning ".ns3. is deprecated" }
  ns4::i = 0; // { dg-warning ".ns4. is deprecated" }
  int i = ns1::i; // { dg-warning ".ns1. is deprecated" }
  int k = ns6::E::X;  // { dg-warning ".ns6. is deprecated" }
  ns7::ns8::x = 42; // { dg-warning ".ns7. is deprecated" }
  N1::N2::N3::N4::x = 42; // { dg-warning ".N1::N2::N3. is deprecated" }
  ns6::fn(); // { dg-warning ".ns6. is deprecated" }
  ns7::S s; // { dg-warning ".ns7. is deprecated" }
  ns7::S sfn(int); // { dg-warning ".ns7. is deprecated" }
  ns7::ns8::Z sfn2(int); // { dg-warning ".ns7. is deprecated" }
}
