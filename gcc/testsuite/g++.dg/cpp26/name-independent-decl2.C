// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

int a[3];

void
foo ()
{
  {
    extern int _ (int);
    int _ = 2;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    extern long _ (long);	// { dg-error "redeclared as different kind of entity" }
  }
  {
    int _ = 3;
    extern int _ (int);		// { dg-error "redeclared as different kind of entity" }
  }
  {
    int _ = 4;
    static int _ = 5;		// { dg-error "redeclaration of 'int _'" }
  }				// { dg-message "static variable is not name-independent" "" { target c++26 } .-1 }
  {
    int _ = 6;
    int _ = 7;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    int _ = 8;
    int _ = 9;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    int _ = 10;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    static int _ = 11;
    static int _ = 12;		// { dg-error "redeclaration of 'int _'" }
    int _ = 13;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  }				// { dg-message "static variable is not name-independent" "" { target c++26 } .-2 }
  {
    extern int _ (int);
    extern long _ (long);
    extern float _ (float);
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    extern double _ (double);
    extern short _ (short);
    int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
    int _ = 2;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    auto [i, _, _] = a;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    auto [i, j, _] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    auto [k, _, l] = a;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
    ++_;			// { dg-error "reference to '_' is ambiguous" }
  }
  {
    static auto [i, _, _] = a;	// { dg-error "redeclaration of 'auto _'" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-2 }
  }				// { dg-message "static structured binding is not name-independent" "" { target c++26 } .-3 }
}

int
bar (int _ = 0)
{
  int _ = 1;			// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  ++_;				// { dg-error "reference to '_' is ambiguous" }
  return 0;
}

void
baz ()
{
  if (int _ = bar ())
    {
      int _ = 6;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  else
    {
      int _ = 7;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  while (int _ = bar ())
    {
      int _ = 8;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
  for (int _ = bar (); _; ++_)
    {
      int _ = 9;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
      ++_;			// { dg-error "reference to '_' is ambiguous" }
    }
}

namespace A
{
  int _ = 1;
  int _ = 1;			// { dg-error "redefinition of 'int A::_'" }
}				// { dg-message "variable at namespace scope is not name-independent" "" { target c++26 } .-1 }

namespace B
{
  auto [_, _, _] = a;		// { dg-error "redefinition of 'auto B::_'" }
				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}				// { dg-message "structured binding at namespace scope is not name-independent" "" { target c++26 } .-2 }

void
qux ()
{
  auto c = [_ = 2, _ = 3] () {	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } .-1 }
    (void) _;			// { dg-error "reference to '_' is ambiguous" }
  };
  {
    int _ = 4;
    auto d = [_, _ = 5] () {	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
				// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } .-1 }
      (void) _;			// { dg-error "reference to '_' is ambiguous" }
    };
  }
  auto e = [_ = 1] (int _) {};	// { dg-warning "lambda capture initializers only available with" "" { target c++11_down } }
}				// { dg-error "lambda parameter '_' previously declared as a capture" "" { target *-*-* } .-1 }

void
corge (int _, int _)		// { dg-error "redefinition of 'int _'" }
{				// { dg-message "parameter declaration is not name-independent" "" { target c++26 } .-1 }
}

namespace C
{
  typedef int _;
  typedef int _;
}

namespace D
{
  namespace {
    int _;
    int _;			// { dg-error "redefinition of 'int D::.anonymous.::_'" }
  }				// { dg-message "variable at namespace scope is not name-independent" "" { target c++26 } .-1 }
}

namespace E
{
  int _ (int);
  int _ (int);
  int _ (int) { return 0; }
  int _ (int) { return 0; }	// { dg-error "redefinition of 'int E::_\\\(int\\\)'" }
  long _ (long) { return 1; }
}

template <int _, int _>		// { dg-error "redefinition of 'int _'" }
void
garply ()
{
}

#if __cpp_concepts >= 202002L
template <typename T>
concept F = requires (T _, T _) { T{}; };	// { dg-error "redefinition of 'T _'" "" { target c++20 } }
#endif						// { dg-message "parameter declaration is not name-independent" "" { target c++26 } .-1 }
