// PR c++/116162
// { dg-do compile { target c++11 } }

struct C0 {
  C0(C0&) = default;
};

struct C1 {
  C1(volatile C1&) = default; // { dg-warning "implicitly deleted" "" { target c++20 } }
			      // { dg-error "does not match" "" { target c++17_down } .-1 }
};

struct C2 {
  C2(const C2&) = default;
};

struct C3 {
  C3(const volatile C3&) = default;  // { dg-warning "implicitly deleted" "" { target c++20 } }
				      // { dg-error "does not match" "" { target c++17_down } .-1 }
};

struct M0 {
  M0(M0&&) = default;
};

struct M1 {
  M1(const M1&&) = default; // { dg-warning "implicitly deleted" "" { target c++20 } }
			    // { dg-error "does not match" "" { target c++17_down } .-1 }
};

struct M2 {
  M2(volatile M2&&) = default;	// { dg-warning "implicitly deleted" "" { target c++20 } }
				// { dg-error "does not match" "" { target c++17_down } .-1 }
};

struct M3 {
  M3(const volatile M3&&) = default;  // { dg-warning "implicitly deleted" "" { target c++20 } }
				      // { dg-error "does not match" "" { target c++17_down } .-1 }
};
