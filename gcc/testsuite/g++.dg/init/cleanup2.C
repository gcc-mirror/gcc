// PR c++/12526

// We decided that the call to strcmp has no side-effects because strcmp is
// pure, even though the first argument has side-effects.  As a result, we
// stripped the CLEANUP_POINT_EXPR.  Hilarity ensued.

extern "C" int strcmp (const char *, const char *);

struct A {
  A(int);
  const char *str();
  ~A();
};

void printQueryI()
{
  if(!strcmp(A(1).str(), "foo"))
    { }
}
