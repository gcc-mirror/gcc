// If a conversion can be interpreted in more than one of the ways listed
// above, the interpretation that appears first in the list is used, even if a
// cast resulting from that interpretation is ill-formed.

struct J23B1 { operator int*(); }; // { dg-message "candidate" }
struct J23B2 { operator int*(); }; // { dg-message "candidate" }
struct J23 : J23B1, J23B2 { operator int const*(); }; // { dg-message "candidate" }
int test_2_3() {
  J23 j;
  return *(int*)j; // { dg-error "ambiguous" }
}

struct J24B1 { operator char&(); }; // { dg-message "candidate" }
struct J24B2 { operator char&(); }; // { dg-message "candidate" }
struct J24 : J24B1, J24B2 {};
int test_2_4() {
  J24 j;
  return (char&)j; // { dg-error "ambiguous" }
}

struct J25B1 { operator char&() const; }; // { dg-message "candidate" }
struct J25B2 { operator char&() const; }; // { dg-message "candidate" }
struct J25 : J25B1, J25B2 {};
int test_2_5() {
  J25 const j;
  return (char&)j; // { dg-error "ambiguous" }
}

// If a conversion can be interpreted in more than one way as a static_cast
// followed by a const_cast, the conversion is ill-formed.
struct J33 {
  operator int const*(); // { dg-message "candidate" }
  operator int volatile*(); // { dg-message "candidate" }
};
int test_3_3() {
  J33 j;
  return *(int*) j; // { dg-error "ambiguous" }
}

struct J34B1 { operator char const&(); }; // { dg-message "candidate" }
struct J34B2 { operator char const&(); }; // { dg-message "candidate" }
struct J34 : J34B1, J34B2 {};
int test_3_4() {
  J34 j;
  return (char&)j; // { dg-error "ambiguous" }
}
