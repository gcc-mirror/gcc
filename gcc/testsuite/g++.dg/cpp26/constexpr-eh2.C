// C++26 P3068R5 - Allowing exception throwing in constant-evaluation
// { dg-do compile { target c++26 } }
// { dg-require-effective-target exceptions_enabled }

struct S {
  constexpr S () : s (0) {}
  constexpr S (int x) : s (x) { if (x == 42) throw 42; }
  constexpr S (const S &x) : s (x.s) {}
  constexpr ~S () noexcept (false) { if (s == 41) throw 41; }
  constexpr const char *what () const noexcept { return "S"; }
  int s;
};
struct T : public S {
  constexpr T () {}
  constexpr T (int x) : S (x) {}
  constexpr T (const T &x) : S (x.s) {}
  constexpr ~T () {}
  constexpr const char *what () const noexcept { return "T"; }
};
struct U {
  constexpr U () : u (0) {}
  constexpr U (int x) : u (x) {}
  constexpr U (const S &x) : u (0) {}
  constexpr U (const U &x) : u (x.u) { if (u == 42) throw 43; }
  constexpr ~U () {}
  constexpr const char *what () const noexcept { return "U"; }
  int u;
};

constexpr int
foo (int x)
{
  if (x == 1)
    throw 43;
  return x;
}

constexpr int
bar (int x) noexcept	// { dg-error "'std::terminate' called" }
{			// { dg-message "uncaught exception exited from 'noexcept' function 'constexpr int bar\\\(int\\\)'" "" { target *-*-* } .-1 }
  return foo (x);
}

constexpr int
baz (int x)
{
  switch (x)
    {
    case 0: throw 1; break;
    case 1: try { x = bar (x); } catch (...) {} break;		// { dg-message "in 'constexpr' expansion of" }
    case 2: throw S (2); break;
    case 3: try { throw S (42); } catch (int a) { if (a != 42) throw -1; } break;
    case 4: try { S s (41); throw 2; } catch (...) {} break;	// { dg-error "'std::terminate' called" }
    case 5: return 5;						// { dg-message "destructor exited with an exception" "" { target *-*-* } .-1 }
    case 6:
      try
	{
	  throw S (5);
	}
      catch (const T &) {}
      catch (int) {}
      catch (const bool &) {}
      catch (const T **const &) {}
      break;
    case 7: try { constexpr int y = foo (2); } catch (...) {} break;
    case 8:
      try
	{
	  try
	    {
	      throw U ();
	    }
	  catch (U &u)
	    {
	      u.u = 42;
	      throw;
	    }
	}
      catch (U u)					// { dg-error "'std::terminate' called" }
	{						// { dg-message "constructor exited with another exception while entering handler" "" { target *-*-* } .-1 }
	}
      break;
    case 9:
      try
	{
	  throw U (S (41));				// { dg-error "'std::terminate' called" }
	}						// { dg-message "destructor exited with an exception" "" { target *-*-* } .-1 }
      catch (...)
	{
	}
      break;
    }
  return -1;
}

constexpr int
qux (int x)
{
  try { constexpr int y = foo (1); } catch (...) {}	// { dg-error "uncaught exception" }
  return 0;
}

constexpr int a = baz (0);	// { dg-error "uncaught exception" }
constexpr int b = baz (1);	// { dg-message "in 'constexpr' expansion of" }
constexpr int c = baz (2);	// { dg-error "uncaught exception" }
constexpr int d = baz (3);
constexpr int e = baz (4);	// { dg-message "in 'constexpr' expansion of" }
constexpr int f = baz (5);
constexpr int g = baz (6);	// { dg-error "uncaught exception" }
constexpr int h = baz (7);
constexpr int i = baz (8);	// { dg-message "in 'constexpr' expansion of" }
constexpr int j = baz (9);	// { dg-message "in 'constexpr' expansion of" }
