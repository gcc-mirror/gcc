namespace A = B;  // ERROR - unknown namespace

namespace C{}
namespace D = C;  
namespace D {     // ERROR - reopening namespace with alias
  void f();
}

void C::f(){}     // ERROR - previous definition

void D::f(){}     // ERROR - redefinition

namespace E = C::F; // ERROR - unknown namespace
