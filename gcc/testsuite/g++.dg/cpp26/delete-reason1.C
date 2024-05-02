// P2573R2 = delete("should have a reason");
// { dg-do compile { target c++11 } }
// { dg-options "" }

void foo () = delete ("reason");		// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-message "declared here" "" { target *-*-* } .-1 }
struct S {
  void bar () = delete ("another reason");	// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
};						// { dg-message "declared here" "" { target *-*-* } .-1 }
int baz (int) = delete ("yet another reason");	// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
int baz (int);					// { dg-message "declared here" }
template <typename T>
void qux (T) = delete ("some other reason");	// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-message "declared here" "" { target *-*-* } .-1 }
template <typename T>
struct U {
  U () = delete ("my reasons");			// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
  U (int);					// { dg-message "declared here" "" { target *-*-* } .-1 }
  ~U () = delete ("your reasons");		// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
};						// { dg-message "declared here" "" { target *-*-* } .-1 }
template <>
void qux (long long) = delete;			// { dg-message "declared here" }
template <typename T>
void corge (T) = delete;			// { dg-message "declared here" }
template <>
void corge (double) = delete ("their reasons");	// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-message "declared here" "" { target *-*-* } .-1 }

void
test (U<int> &x)
{
  foo ();					// { dg-error "use of deleted function 'void foo\\\(\\\)': reason" }
  S{}.bar ();					// { dg-error "use of deleted function 'void S::bar\\\(\\\)': another reason" }
  baz (0);					// { dg-error "use of deleted function 'int baz\\\(int\\\)': yet another reason" }
  qux (0L);					// { dg-error "use of deleted function 'void qux\\\(T\\\) \\\[with T = long int\\\]': some other reason" }
  qux (0LL);					// { dg-error "use of deleted function 'void qux\\\(T\\\) \\\[with T = long long int\\\]'" }
  U<long> u;					// { dg-error "use of deleted function 'U<T>::U\\\(\\\) \\\[with T = long int\\\]': my reasons" }
						// { dg-error "use of deleted function 'U<T>::~U\\\(\\\) \\\[with T = long int\\\]': your reasons" "" { target *-*-* } .-1 }
  corge (0);					// { dg-error "use of deleted function 'void corge\\\(T\\\) \\\[with T = int\\\]'" }
  corge (0.0);					// { dg-error "use of deleted function 'void corge\\\(T\\\) \\\[with T = double\\\]': their reasons" }
}
