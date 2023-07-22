// { dg-do compile { target c++14 } }
// { dg-options "-Wno-return-local-addr" }

struct Empty {};

constexpr const Empty& empty() {
  return Empty{};  // { dg-message "note: declared here" }
}

constexpr const Empty& empty_parm(Empty e) {  // { dg-message "note: declared here" }
  return e;
}

constexpr Empty a = empty();  // { dg-error "outside its lifetime" }
constexpr Empty b = empty_parm({});  // { dg-error "outside its lifetime" }
