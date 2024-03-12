// PR c/102989
// { dg-do compile { target c++11 } }
// { dg-options "" }

constexpr int operator""wb (unsigned long long) { return 0; }	// { dg-warning "reserved for future standardization" }
constexpr int operator""uwb (unsigned long long) { return 1; }	// { dg-warning "reserved for future standardization" }

static_assert (-42wb == 0, "");
static_assert (81uwb == 1, "");
