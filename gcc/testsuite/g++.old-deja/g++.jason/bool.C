// { dg-do assemble  }
// Testcase for uses of bool.

int i,j,k;

/* Check that types of certain expressions are bool.  */
void f ()
{
  i ? j == k : true;
  i ? j < k : true;
  i ? j && k : true;
}

/* Check that g++ can find a conversion to bool when one exists.  */
struct A { operator char * (); } a;
struct B { operator int (); } b;
struct C { operator float (); } c;
struct D { operator bool (); } d;
struct E { operator int E::* (); } e;

void g ()
{
  a || true;
  b || true;
  c || true;			// { dg-bogus "" } 
  d || true;
  e || true;
}

/* Check for support in templates.  */
template <class T> struct F { };
template class F<bool>;

template <class T> void f (T, bool) { }
template void f (bool, bool);

/* Special cases.  */
void h ()
{
  /* Used to cause infinite recursion.  */
  i&1 || true;
  /* Should find conversion path to int.  */
  d == true;
}

bool boo = -1;
