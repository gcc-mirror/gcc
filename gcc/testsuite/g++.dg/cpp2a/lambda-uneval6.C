// { dg-do compile { target c++2a } }

static decltype([] { }) f();
static decltype([] { }) f(); // { dg-error "ambiguating" }

static decltype([] { }) g();
static decltype(g()) g(); // okay

static void h(decltype([] { }) *) { }
static void h(decltype([] { }) *) { }
void x1() { h(nullptr); } // { dg-error "ambiguous" }

using A = decltype([] { });
static void i(A *);
static void i(A *) { }
void x2() { i(nullptr); } // okay

template <typename T>
using B = decltype([] { });
static void j(B<char16_t> *) { }
static void j(B<char32_t> *) { }
void x3() { j(nullptr); } // { dg-error "ambiguous" }

template <int N> static void k(decltype([]{ return 0; }()));
template <int N> static void k(decltype([]{ return 0; }())); // okay
template <int N> static void k(int); // okay
