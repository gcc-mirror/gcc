// P0847R7
// { dg-do compile { target c++23 } }

// rejection and diagnosis of an incorrectly declared xobj parameter

// default argument

struct S {
  void f0(this S = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }
  void f1(this S = {}); // { dg-error "an explicit object parameter may not have a default argument" }
  void f2(this S);
  void f10(this S s = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }
  void f11(this S s = {}); // { dg-error "an explicit object parameter may not have a default argument" }
  void f12(this S s);
};

void S::f1(this S) {}
void S::f2(this S = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }

void S::f11(this S s) {}
void S::f12(this S s = {}) {} // { dg-error "an explicit object parameter may not have a default argument" }

// parameter pack

struct S0 {
  template<typename Selves>
  void f(this Selves...) {}

  template<typename Selves>
  void g(this Selves... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				    // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void h(this auto...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void j(this auto... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void k(this Selves...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename Selves>
  void fd(this Selves...);

  template<typename Selves>
  void gd(this Selves... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				   // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void hd(this auto...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void jd(this auto... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void kd(this Selves...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
};

struct S1 {
  template<typename Selves>
  void f(this Selves&...) {}

  template<typename Selves>
  void g(this Selves&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				     // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void h(this auto&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void j(this auto&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void k(this Selves&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename Selves>
  void fd(this Selves&...);

  template<typename Selves>
  void gd(this Selves&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				    // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void hd(this auto&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void jd(this auto&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void kd(this Selves&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
};

struct S2 {
  template<typename Selves>
  void f(this Selves&&...) {}

  template<typename Selves>
  void g(this Selves&&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				      // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void h(this auto&&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void j(this auto&&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void k(this Selves&&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename Selves>
  void fd(this Selves&&...);

  template<typename Selves>
  void gd(this Selves&&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
				     // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void hd(this auto&&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void jd(this auto&&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void kd(this Selves&&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
};

struct S3 {
  template<typename Selves>
  void f(this Selves const&...) {}

  template<typename Selves>
  void g(this Selves const&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
					   // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void h(this auto const&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void j(this auto const&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void k(this Selves const&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename Selves>
  void fd(this Selves const&...);

  template<typename Selves>
  void gd(this Selves const&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
					  // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void hd(this auto const&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void jd(this auto const&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void kd(this Selves const&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
};

struct S4 {
  template<typename Selves>
  void f(this Selves const&&...) {}

  template<typename Selves>
  void g(this Selves const&&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
					    // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void h(this auto const&&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void j(this auto const&&... selves) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void k(this Selves const&&...) {}  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename Selves>
  void fd(this Selves const&&...);

  template<typename Selves>
  void gd(this Selves const&&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
					   // { dg-error "contains no parameter packs" "" { target *-*-* } .-1 }

  void hd(this auto const&&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
  void jd(this auto const&&... selves);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }

  template<typename ...Selves>
  void kd(this Selves const&&...);  // { dg-error "an explicit object parameter cannot be a function parameter pack" }
};
