// PR c++/48324
// { dg-do compile { target c++11 } }

struct S {
  const int val;
  constexpr S(int i) : val(i) { }
};

constexpr const int& to_ref(int i) {
  return S(i).val; // { dg-warning "reference to temporary" }
}

constexpr int ary[to_ref(98)] = { }; // { dg-error "not an integral" }
