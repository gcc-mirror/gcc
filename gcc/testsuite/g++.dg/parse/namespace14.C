// PR c++/78637

namespace X {
class Y;
}
namespace X::Y z;  // { dg-error "namespace|expected|type" }
