// Ensure that generic lambdas properly construct and destroy user types.
// { dg-options "-DUSE_AUTO_SYNTAX" }
// { dg-do run { target c++14 } }

int i = 3;

struct S
{
  S () { ++i; }
  S (S const&) { ++i; }
  S (S&& old) { old.shadow = true; i += 2; }
  ~S () { if (shadow) i -= 2; else --i; }

  bool shadow = false;
};

extern "C" int printf(const char*, ...);
#define assert(e) if (e); else \
		 printf ("%s:%d: !(%s)\n", __FILE__, __LINE__, #e), __builtin_abort ();

int main ()
{
  assert (i == 3);
  {
    S s; assert (i == 4);

    #if USE_AUTO_SYNTAX
    auto byref = [] (auto& r)                   { (void) r; };
    auto bycref = [] (auto const& r)            { (void) r; };
    auto byval = [] (auto v, auto const x)      { assert (i == x); (void) v; };
    auto byrval = [] (auto&& r, auto const x)   { S steal (static_cast<S&&>(r));
		 		 		 		 		           assert (i == x); };

    #elif USE_EXPLICIT_TEMPLATE_SYNTAX
    auto byref = [] <typename T> (T& r)         { (void) r; };
    auto bycref = [] <typename T> (T const& r)  { (void) r; };
    auto byval = [] <typename T, typename I>
		 		     (T v, I const x)		 		 { assert (i == x); (void) v; };
    auto byrval = [] <typename T, typename I>
		 		      (T&& r, I const x)		 		 { S steal (static_cast<S&&>(r));
		 		 		 		 		           assert (i == x); };
    #endif

    byref (s); assert (i == 4);
    bycref (s); assert (i == 4);
    byval (s, 5); assert (i == 4);
    byrval (static_cast<S&&>(s), 6); assert (i == 5);
  }
  assert (i == 3);
}

