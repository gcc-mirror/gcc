// Bug: g++ crashes on this input.
// Build don't link:

struct A {
  const char *p;
};
const char foo[] = "bar";
const A a = { foo };
extern const A* ap = &a;
