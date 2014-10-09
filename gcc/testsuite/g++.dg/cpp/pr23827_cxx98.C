// { dg-do run { target c++98_only } }
// { dg-options "-ansi -pedantic-errors" }

#define f (
#define l )
#define str(x) #x
#define xstr(x) str(x)

// C90 and C++98: "0x1p+( 0x1p+)"
// C99 and C++11: "0x1p+f 0x1p+l"
const char *s = xstr(0x1p+f 0x1p+l);

extern "C" void abort (void);
extern "C" int strcmp (const char *, const char *);

int
main()
{
  if (strcmp (s, "0x1p+( 0x1p+)"))
    abort (); // Correct C99 and C++11 behavior.
  else
    return 0; // Correct C90 and C++ behavior.
}
