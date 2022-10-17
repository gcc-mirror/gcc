// PR c++/48396

namespace std {
  type_info *p;			// { dg-error "type_info" }
}
