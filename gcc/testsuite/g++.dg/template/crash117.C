// PR c++/58448

class SmallVector; struct Types4;
template <typename, typename, typename, typename> struct Types {
  typedef Types4<>::Constructable // { dg-error "template|typedef|expected" }
} Types<SmallVector, SmallVector, SmallVector, SmallVector>:: > // { dg-error "expected" }
