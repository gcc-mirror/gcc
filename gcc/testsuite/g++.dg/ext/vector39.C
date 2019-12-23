// { dg-options "-flax-vector-conversions" }

typedef unsigned char v16qi __attribute__((vector_size(16)));
typedef unsigned int v4si __attribute__((vector_size(16)));

extern v4si normal_v4si;
extern const v4si const_v4si;
extern v4si *normal_v4si_ptr;
extern const v4si *const_v4si_ptr;
extern v4si &normal_v4si_ref;
extern const v4si &const_v4si_ref;

extern v16qi normal_v16qi;
extern const v16qi const_v16qi;
extern v16qi *normal_v16qi_ptr;
extern const v16qi *const_v16qi_ptr;
extern v16qi &normal_v16qi_ref;
extern const v16qi &const_v16qi_ref;

namespace nonconst_refs {
  v16qi &ref_normal_v4si = normal_v4si; // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_const_v4si = const_v4si; // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_normal_v4si_ptr = *normal_v4si_ptr; // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_const_v4si_ptr = *const_v4si_ptr; // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_normal_v4si_ref = normal_v4si_ref; // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_const_v4si_ref = const_v4si_ref; // { dg-error {cannot bind non-const lvalue} }

  v16qi &ref_normal_v16qi = normal_v16qi;
  v16qi &ref_const_v16qi = const_v16qi; // { dg-error {discards qualifiers} }
  v16qi &ref_normal_v16qi_ptr = *normal_v16qi_ptr;
  v16qi &ref_const_v16qi_ptr = *const_v16qi_ptr; // { dg-error {discards qualifiers} }
  v16qi &ref_normal_v16qi_ref = normal_v16qi_ref;
  v16qi &ref_const_v16qi_ref = const_v16qi_ref; // { dg-error {discards qualifiers} }
}

#if __cplusplus >= 201103L
namespace nonconst_rvalue_refs {
  v16qi &&ref_normal_v4si = normal_v4si;
  v16qi &&ref_const_v4si = const_v4si;
  v16qi &&ref_normal_v4si_ptr = *normal_v4si_ptr;
  v16qi &&ref_const_v4si_ptr = *const_v4si_ptr;
  v16qi &&ref_normal_v4si_ref = normal_v4si_ref;
  v16qi &&ref_const_v4si_ref = const_v4si_ref;

  v16qi &&ref_normal_v16qi = normal_v16qi; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  v16qi &&ref_const_v16qi = const_v16qi; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  v16qi &&ref_normal_v16qi_ptr = *normal_v16qi_ptr; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  v16qi &&ref_const_v16qi_ptr = *const_v16qi_ptr; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  v16qi &&ref_normal_v16qi_ref = normal_v16qi_ref; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  v16qi &&ref_const_v16qi_ref = const_v16qi_ref; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
}
#endif

namespace const_refs {
  const v16qi &ref_normal_v4si = normal_v4si;
  const v16qi &ref_const_v4si = const_v4si;
  const v16qi &ref_normal_v4si_ptr = *normal_v4si_ptr;
  const v16qi &ref_const_v4si_ptr = *const_v4si_ptr;
  const v16qi &ref_normal_v4si_ref = normal_v4si_ref;
  const v16qi &ref_const_v4si_ref = const_v4si_ref;

  const v16qi &ref_normal_v16qi = normal_v16qi;
  const v16qi &ref_const_v16qi = const_v16qi;
  const v16qi &ref_normal_v16qi_ptr = *normal_v16qi_ptr;
  const v16qi &ref_const_v16qi_ptr = *const_v16qi_ptr;
  const v16qi &ref_normal_v16qi_ref = normal_v16qi_ref;
  const v16qi &ref_const_v16qi_ref = const_v16qi_ref;
}

#if __cplusplus >= 201103L
namespace const_rvalue_refs {
  const v16qi &&ref_normal_v4si = normal_v4si;
  const v16qi &&ref_const_v4si = const_v4si;
  const v16qi &&ref_normal_v4si_ptr = *normal_v4si_ptr;
  const v16qi &&ref_const_v4si_ptr = *const_v4si_ptr;
  const v16qi &&ref_normal_v4si_ref = normal_v4si_ref;
  const v16qi &&ref_const_v4si_ref = const_v4si_ref;

  const v16qi &&ref_normal_v16qi = normal_v16qi; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  const v16qi &&ref_const_v16qi = const_v16qi; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  const v16qi &&ref_normal_v16qi_ptr = *normal_v16qi_ptr; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  const v16qi &&ref_const_v16qi_ptr = *const_v16qi_ptr; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  const v16qi &&ref_normal_v16qi_ref = normal_v16qi_ref; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
  const v16qi &&ref_const_v16qi_ref = const_v16qi_ref; // { dg-error {cannot bind rvalue reference} "" { target c++11 } }
}
#endif

namespace rvalue_reinterpret_refs {
  v16qi &ref_normal_v4si = reinterpret_cast<v16qi>(normal_v4si); // { dg-error {cannot bind non-const lvalue} }
  v16qi &ref_const_v4si = reinterpret_cast<v16qi>(const_v4si); // { dg-error {cannot bind non-const lvalue} }
}

namespace ref_reinterpret_refs {
  v16qi &ref_normal_v4si = reinterpret_cast<v16qi &>(normal_v4si);
  v16qi &ref_const_v4si = reinterpret_cast<v16qi &>(const_cast<v4si &>(const_v4si));
}
