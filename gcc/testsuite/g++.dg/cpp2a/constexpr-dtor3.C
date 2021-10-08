// P0784R7
// { dg-do compile { target c++20 } }

struct S
{
  constexpr S () : s (0) {}
  constexpr ~S () {}
  int s;
};
struct T	// { dg-message "'T' is not literal because" }
{		// { dg-message "'T' does not have 'constexpr' destructor" "" { target *-*-* } .-1 }
  constexpr T () : t (0) {}
  ~T () {}	// { dg-message "defaulted destructor calls non-'constexpr' 'T::~T\\(\\)'" }
  int t;
};
struct U : public S
{
  constexpr U () : u (0) {}
  constexpr ~U () = default;	// { dg-error "explicitly defaulted function 'constexpr U::~U\\(\\)' cannot be declared 'constexpr' because the implicit declaration is not 'constexpr'" }
  int u;
  T t;
};
struct V : virtual public S
{
  V () : v (0) {}
  constexpr ~V () = default;	// { dg-error "explicitly defaulted function 'constexpr V::~V\\(\\)' cannot be declared 'constexpr' because the implicit declaration is not 'constexpr'" }
  int v;
};
struct W0
{
  constexpr W0 () : w (0) {}
  constexpr W0 (int x) : w (x) {}
  constexpr ~W0 () { if (w == 5) asm (""); w = 3; }
  int w;
};
struct W1
{
  constexpr W1 () : w (0) {}
  constexpr W1 (int x) : w (x) {}
  constexpr ~W1 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W2
{
  constexpr W2 () : w (0) {}
  constexpr W2 (int x) : w (x) {}
  constexpr ~W2 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W3
{
  constexpr W3 () : w (0) {}
  constexpr W3 (int x) : w (x) {}
  constexpr ~W3 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W4
{
  constexpr W4 () : w (0) {}
  constexpr W4 (int x) : w (x) {}
  constexpr ~W4 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W5
{
  constexpr W5 () : w (0) {}
  constexpr W5 (int x) : w (x) {}
  constexpr ~W5 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W6
{
  constexpr W6 () : w (0) {}
  constexpr W6 (int x) : w (x) {}
  constexpr ~W6 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W7
{
  constexpr W7 () : w (0) {}
  constexpr W7 (int x) : w (x) {}
  constexpr ~W7 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct W8
{
  constexpr W8 () : w (0) {}
  constexpr W8 (int x) : w (x) {}
  constexpr ~W8 () { if (w == 5) asm (""); w = 3; }	// { dg-error "inline assembly is not a constant expression" }
							// { dg-message "only unevaluated inline assembly is allowed in a 'constexpr' function" "" { target *-*-* } .-1 }
  int w;
};
struct X : public T
{
  constexpr X () : x (0) {}
  constexpr ~X () = default;	// { dg-error "explicitly defaulted function 'constexpr X::~X\\(\\)' cannot be declared 'constexpr' because the implicit declaration is not 'constexpr'" }
  int x;
};
constexpr S s;
constexpr T t;	// { dg-error "the type 'const T' of 'constexpr' variable 't' is not literal" }
constexpr W0 w1;
constexpr W0 w2 = 12;
constexpr W1 w3 = 5;	// { dg-message "in 'constexpr' expansion of" }
constexpr W0 w4[3] = { 1, 2, 3 };
constexpr W2 w5[3] = { 4, 5, 6 };	// { dg-message "in 'constexpr' expansion of" }

void
f1 ()
{
  constexpr S s2;
  constexpr W0 w6;
  constexpr W0 w7 = 12;
  constexpr W3 w8 = 5;	// { dg-message "in 'constexpr' expansion of" }
  constexpr W0 w9[3] = { 1, 2, 3 };
  constexpr W4 w10[3] = { 4, 5, 6 };	// { dg-message "in 'constexpr' expansion of" }
}

constexpr int
f2 ()
{
  constexpr S s3;
  constexpr W0 w11;
  constexpr W0 w12 = 12;
  constexpr W5 w13 = 5;	// { dg-message "in 'constexpr' expansion of" }
  constexpr W0 w14[3] = { 1, 2, 3 };
  constexpr W6 w15[3] = { 4, 5, 6 };	// { dg-message "in 'constexpr' expansion of" }
  return 0;
}

constexpr int
f3 ()
{
  S s3;
  W0 w11;
  W0 w12 = 12;
  W0 w14[3] = { 1, 2, 3 };
  return 0;
}

constexpr int x3 = f3 ();

constexpr int
f4 ()
{
  W7 w13 = 5;
  return 0;
}			// { dg-message "in 'constexpr' expansion of" }

constexpr int x4 = f4 ();	// { dg-message "in 'constexpr' expansion of" }

constexpr int
f5 ()
{
  W8 w15[3] = { 4, 5, 6 };	// { dg-message "in 'constexpr' expansion of" }
  return 0;
}

constexpr int x5 = f5 ();	// { dg-message "in 'constexpr' expansion of" }

void
f6 ()
{
  constexpr T t2;	// { dg-error "the type 'const T' of 'constexpr' variable 't2' is not literal" }
}

constexpr int
f7 ()
{
  constexpr T t3;	// { dg-error "the type 'const T' of 'constexpr' variable 't3' is not literal" }
  return 0;
}

constexpr int
f8 ()
{
  T t4;			// { dg-error "variable 't4' of non-literal type 'T' in 'constexpr' function only available with" "" { target c++20_down } }
  return 0;
}
