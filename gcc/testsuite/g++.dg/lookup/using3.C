// PR c++/9798

namespace std { }
namespace STL { using namespace std; }
namespace std {
  using namespace STL;
}
namespace STL {
  struct A {
    void B() { using namespace std; }
  };
}
