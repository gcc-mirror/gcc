// PR c++/14821

namespace A {
  namespace B {}
}

namespace A {
  namespace Alias = ::A::B;
}

namespace A {
  namespace Alias = ::A::B;
}
