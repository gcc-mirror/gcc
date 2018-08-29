// PR c++/84611

template<typename = int>
struct a {
  a() {
    struct c;
    try {
    } catch (struct c {}) {  // { dg-error "types may not be defined|conflicting" }
    }
  }
};

struct d {
  d();
  a<> b;
};

d::d() {}
