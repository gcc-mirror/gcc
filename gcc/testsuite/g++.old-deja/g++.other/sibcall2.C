// { dg-do assemble  }
// { dg-options "-O2 -foptimize-sibling-calls -fno-exceptions" }

struct X
{
  const char *c() const { return b; };
  char *b;
};
extern "C" int f (const char *);
struct A
{
   X x;
   void y();
};
void A::y()
{
  const char *const a[] = { x.c() };
  f (a[0]);
}
