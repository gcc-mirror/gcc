// simple test for id from base class during class defn
// Build don't link:

struct foo {
  enum { blah = 1 };
};
struct bar : public foo {
  char cache[blah];
};
