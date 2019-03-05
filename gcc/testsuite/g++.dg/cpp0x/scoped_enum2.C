// { dg-do compile { target c++11 } }

enum class E { e = 10 };
enum E2 { e2 = 10 };

struct C {
  int arr[E::e];    // { dg-error "could not convert" }
// { dg-error "14:size of array .arr. has non-integral" "" { target c++11 } .-1 }
  int arr2[E2::e2]; // OK
  int i: E::e;	    // { dg-error "could not convert|non-integral type" }
  int i2: E2::e2;   // OK
};
