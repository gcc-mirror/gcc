// { dg-do compile }
// { dg-options "-std=c++2a" }

consteval int bar () { return 42; }
consteval int baz () { return 1; }
typedef int (*fnptr) ();
consteval fnptr quux () { return bar; }

void
foo ()
{
   auto qux = [] (fnptr a = quux ()) consteval { return a (); };
   constexpr auto c = qux (baz);	// { dg-error "28:taking address of an immediate function" }
   constexpr auto d = qux (bar);	// { dg-error "28:taking address of an immediate function" }
   static_assert (c == 1);
   static_assert (d == 42);
}
