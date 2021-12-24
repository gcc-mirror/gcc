// { dg-do run }
// { dg-options "-Wconversion" }
// The conversions performed by
// (4.1) a const_cast,
// (4.2) a static_cast,
// (4.3) a static_cast followed by a const_cast,
// (4.4) a reinterpret_cast, or
// (4.5) a reinterpret_cast followed by a const_cast,
// can be performed using the cast notation of explicit type conversion.

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

int test_1_2() {
  int i = 2;
  const_cast<int&>((int const&)i) = 1;
  return i;
}

int test_1_3() {
  int i = 3;
  int const& r = i;
  (int&) r = 1;
  return i;
}

struct J23 {
  int i2, i3;
  operator int*() { return &i2; }
  operator int const*() { return &i3; }
};
int test_2_3() {
  J23 j = {2, 3};
  return *(int*)j;
}

int test_2_4() {
  int i = 2;
  int const& r = i;
  const_cast<unsigned&>((unsigned const&)r) = 4; // { dg-warning "reference binding temporary" }
  // { dg-message "reinterpret_cast" "" { target *-*-* } .-1 }
  return i;
}

int test_2_5() {
  int i = 2;
  const_cast<unsigned&>((unsigned const&)i) = 5; // { dg-warning "reference binding temporary" }
  // { dg-message "reinterpret_cast" "" { target *-*-* } .-1 }
  return i;
}

int test_3_4() {
  int i = 3;
  (unsigned&)i = 4; // { dg-warning "reference binding temporary" }
  // { dg-message "reinterpret_cast" "" { target *-*-* } .-1 }
  return i;
}

int test_3_5() {
  int i = 3;
  int const& r = i;
  (unsigned&)r = 5; // { dg-warning "reference binding temporary" }
  // { dg-message "reinterpret_cast" "" { target *-*-* } .-1 }
  return i;
}

int main() {
  assert(test_1_2() == 1);
  assert(test_1_3() == 1);
  //assert(test_1_4() == 1);
  //assert(test_1_5() == 1);
  assert(test_2_3() == 2);
  assert(test_2_4() == 2);
  assert(test_2_5() == 2);
  assert(test_3_4() == 3);
  assert(test_3_5() == 3);
  //assert(test_4_5() == 4);
}
