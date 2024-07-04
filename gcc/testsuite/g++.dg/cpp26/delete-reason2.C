// P2573R2 = delete("should have a reason");
// { dg-do compile { target c++11 } }
// { dg-options "" }

void foo () = delete (;				// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "expected string-literal before ';' token" "" { target *-*-* } .-1 }
						// { dg-error "expected '\\\)' before ';' token" "" { target *-*-* } .-2 }
void bar () = delete ();			// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "expected string-literal before '\\\)' token" "" { target *-*-* } .-1 }
void baz () = delete (0);			// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "expected string-literal before numeric constant" "" { target *-*-* } .-1 }
						// { dg-error "expected '\\\)' before numeric constant" "" { target *-*-* } .-2 }
						// { dg-error "expected ',' or ';' before numeric constant" "" { target *-*-* } .-3 }
void qux () = delete (L"");			// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "a wide string is invalid in this context before '\\\)' token" "" { target *-*-* } .-1 }
void corge () = delete (u8"");			// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "a wide string is invalid in this context before '\\\)' token" "" { target *-*-* } .-1 }
void garply () = delete ("something" + 0);	// { dg-warning "'delete' reason only available with" "" { target c++23_down } }
						// { dg-error "expected '\\\)' before '\\\+' token" "" { target *-*-* } .-1 }
						// { dg-error "expected ',' or ';' before '\\\+' token" "" { target *-*-* } .-2 }
