// PR c++/12253
// We should not destroy the temporary A passed to the
// constructor for b[0] before going on to construct b[1].

// { dg-do run }

extern "C" int printf (const char *, ...);

int c;
int r;

struct A
{
  A() { printf ("A()\n"); ++c; }
  A(const A&) { printf ("A(const A&)\n"); ++c; }
  ~A() { printf ("~A()\n"); --c; }
};
 
struct B
{
  B(int i, const A& = A()) {
    printf ("B()\n");
    if (c != i) r = 1;
  }
};
 
int main()
{
  B b[] = { 1, 2 };
  return r;
}
