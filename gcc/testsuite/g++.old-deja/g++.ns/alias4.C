// { dg-do assemble  }
namespace A = B;  // { dg-error "" } unknown namespace

namespace C{}
namespace D = C;  
namespace D {     // { dg-error "" } reopening namespace with alias
  void f();
}

void C::f(){}     // { dg-message "" } previous definition

void D::f(){}     // { dg-error "" } redefinition

namespace E = C::F; // { dg-error "" } unknown namespace
