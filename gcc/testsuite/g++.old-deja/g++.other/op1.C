// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S {
  bool operator! (int, ...); // ERROR - 
};
