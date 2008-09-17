// { dg-do assemble  }
// Bug: g++ crashes on this input.

struct A {
  const char *p;
};
const char foo[] = "bar";
const A a = { foo };
extern const A* ap = &a; //{ dg-warning "'ap' initialized and declared 'extern'" }
