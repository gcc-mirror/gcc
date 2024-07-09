// { dg-do compile { target c++11 } }

constexpr int&& r = 1 + 2;  // { dg-message "pointed-to object declared here" "" { target c++26 } }
constexpr void* vpr = &r;
constexpr int* pi = static_cast<int*>(vpr);  // { dg-error "cast from .void\\*. is not allowed" "" { target c++23_down } }
constexpr float* pf = static_cast<float*>(vpr);  // { dg-error "cast from .void\\*. is not allowed" "" { target c++23_down } }
// { dg-error "cast from .void\\*. is not allowed in a constant expression because pointed-to type .int. is not similar to .float." "" { target c++26 } .-1 }

constexpr void* vnp = nullptr;
constexpr int* pi2 = static_cast<int*>(vnp);  // { dg-error "cast from .void\\*. is not allowed" "" { target c++23_down } }
