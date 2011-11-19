// PR c++/28211

template <const int*> class Helper { };
const int foo = 0;
typedef Helper<&foo> HelperType; // { dg-error "linkage|type" "" { target c++98 } }
