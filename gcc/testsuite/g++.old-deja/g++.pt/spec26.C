// From: lat@iki.fi (Lassi A. Tuura)
// Test that a specialization without an initializer is not a definition,
// as specified in [temp.expl.spec].

// Build don't link:

struct X;
template <class T> struct Y { static const X array[]; };
template <> const X Y<int>::array [];
struct X { int i; };
template <> const X Y<int>::array [] = { 0, 1, 2, 3 };
