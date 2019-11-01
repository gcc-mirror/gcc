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
   constexpr auto e = qux ();
   static_assert (e == 42);
}
