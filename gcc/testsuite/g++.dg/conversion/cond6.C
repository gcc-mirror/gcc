// PR c++/11283
// Converting "a" to the type of "i" produces "int" rather than "const
// int", which was causing build_conditional_expr to abort.  But we don't
// care about cv-quals on non-class rvalues.

struct A
{
  operator int ();
};

extern A a;
extern const int i;
extern bool b;

int f ()
{
  return b ? a : i;
}
