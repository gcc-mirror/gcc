// Build don't link: 
// GROUPS passed error-reporting
// Special g++ Options: -Wreturn-type

// DR 295 allows qualification via typedef

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
}

int foo2 (int (*a)(int) = &foo)
{
   undef4 (1); // ERROR - implicit declaration
  return 1;
}

class X{
  class Y{};			// ERROR - private
};

typedef int const * bart ();
//The following is DR295 dependant
typedef bart const * const * bar2; // ERROR - constifying qualifiers
typedef bart volatile * const * bar2v; // ERROR - qualifiers

bar2 baz (X::Y y)
{				// ERROR - in this context
  X::Y f;			// ERROR - in this context
  bar2 wa [5];
  wa[0] = baz(f);
  undef2 (1); // ERROR - implicit declaration
}

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
}
