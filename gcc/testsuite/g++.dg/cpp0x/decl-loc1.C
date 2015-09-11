// { dg-do compile { target c++11 } }

inline struct A;      // { dg-error "1:'inline'" }
virtual struct B;     // { dg-error "1:'virtual'" }
friend struct C;      // { dg-error "1:'friend'" }
explicit struct D;    // { dg-error "1:'explicit'" }
mutable struct E;     // { dg-error "1:a storage class" }
const struct F;       // { dg-error "1:'const'" }
volatile struct G;    // { dg-error "1:'volatile'" }
__restrict struct H;  // { dg-error "1:'__restrict'" }
__thread struct I;    // { dg-error "1:'__thread'" }
typedef struct J;     // { dg-warning "1:'typedef'" }
constexpr struct K;   // { dg-error "1:'constexpr'" }
