// PR c++/58724
// { dg-do compile { target c++11 } }

namespace foo __attribute__((visibility("default"))) {}
namespace [[gnu::visibility("default")]] bar {}
