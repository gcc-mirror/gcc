// PR c++/12253
// Bug: We were failing to destroy the temporary A passed to the
// constructor for b[0] before going on to construct b[1].

// { dg-do run }

extern "C" int printf (const char *, ...);

int c;
int r;

struct A
{
  A() { printf ("A()\n"); if (c++) r = 1; }
  A(const A&) { printf ("A(const A&)\n"); ++c; }
  ~A() { printf ("~A()\n"); --c; }
};
 
struct B
{
  B(int, const A& = A()) { printf ("B()\n"); }
};
 
int main()
{
  B b[] = { 0, 0 };
  return r;
}
