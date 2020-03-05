// PR 91826 bogus error with aliased namespace

namespace N1 { class C1; }
namespace A1 = N1;
class A1::C1 {}; //Ok

namespace N2
{
  namespace N { class C2; }
  namespace A2 = N;
  class A2::C2 {}; // { dg_bogus "does not enclose" }
}

namespace N3 { namespace N { class C3; } }
namespace A3 = N3::N;
class A3::C3 {}; //Ok
