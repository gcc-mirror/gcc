// PR c++/97202
// { dg-options "" }

template<typename Base> struct S : Base {
  inline S<Base>() {} // { dg-warning "template-id not allowed for constructor" "" { target c++20 } }
  inline ~S<Base>() {} // { dg-warning "template-id not allowed for destructor" "" { target c++20 } }
};
