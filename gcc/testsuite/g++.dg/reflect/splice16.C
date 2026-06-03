// PR c++/125069
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -Wno-error=missing-template-keyword" }

#include <meta>
struct x{ x(auto); };

constexpr auto ac = std::meta::access_context::unchecked();

template<auto r>
constexpr auto t = (
  [:r:],  // { dg-error "cannot use constructor or destructor" }
  &[:r:],  // { dg-error "cannot use constructor or destructor" }
  template [:r:], // { dg-error "cannot use constructor or destructor" }
  &template [:r:], // { dg-error "cannot use constructor or destructor" }
  template [:r:]<int>, // { dg-error "cannot use constructor or destructor" }
  &template [:r:]<int>, // { dg-error "cannot use constructor or destructor" }
  1);


int main() {
  constexpr auto r = members_of(^^x, ac)[0];

  t<r>;

  [:r:];  // { dg-error "cannot use constructor or destructor" }
  &[:r:];  // { dg-error "cannot use constructor or destructor" }
  [:r:]<int>; // { dg-error "cannot use constructor or destructor" }
  &[:r:]<int>; // { dg-error "cannot use constructor or destructor" }
  template [:r:]; // { dg-error "cannot use constructor or destructor" }
  &template [:r:]; // { dg-error "cannot use constructor or destructor" }
  template [:r:]<int>; // { dg-error "cannot use constructor or destructor" }
  &template [:r:]<int>; // { dg-error "cannot use constructor or destructor" }
}
