// Build don't link: 
// GROUPS passed error-reporting
template <char C>
class badoo
{
};

template <int (*F) (int)>
class doowop
{
};

struct A
{
  int a;
  ~A () { a = 0; }
  operator int () { return a; }
};

extern "C" int atoi (char *);

int (*fee)(char *) = atoi;
int (**bar)(char *) = &fee;

char *s = "4";
char **sp = &s;
char ***spp = &sp;

int foo (int a = (**bar) (s))
{
   return doowop<foo>::bar; // ERROR - not a member
} // ERROR - non-void

int foo2 (int (*a)(int) = &foo)
{
   undef4 (1); // ERROR - implicit declaration
  return 1;
}

class X{
  class Y{};			// ERROR - private
};

typedef int const * bart ();
typedef bart const * const * bar2; // ERROR - qualifiers

bar2 baz (X::Y y)
{				// ERROR - in this context
  X::Y f;			// ERROR - in this context
  bar2 wa [5];
  wa[0] = baz(f);
  undef2 (1); // ERROR - implicit declaration
} // ERROR - non-void

int ninny ()
{
  struct A
    {
	  static int ninny2 () { return badoo<'\001'>::foo; } // ERROR - not a member
    };

  return A::ninny2();
}

int darg (char X::*p)
{
   undef3 (1); // ERROR - implicit declaration
} // ERROR - non-void
