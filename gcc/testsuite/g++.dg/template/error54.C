// PR c++/56380

template <typename T>
struct X {
  X();
  mutable T x;  // { dg-error "cannot be declared" }
};

X<const int> a; // { dg-message "required from here" }
X<int&> b;      // { dg-message "required from here" }
