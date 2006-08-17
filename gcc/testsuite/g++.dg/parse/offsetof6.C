/* { dg-do compile } */

// From PR28573

struct A
{
  char d[44];
  char &operator [] ( int indx ) { return d[indx]; }
};

struct B
{
  A a;
};

int main()
{
  return __builtin_offsetof(B, a[0]); /* { dg-error "cannot apply.*offsetof" } */
}
