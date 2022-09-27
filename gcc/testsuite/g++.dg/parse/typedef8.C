//PR c++ 29024

typedef static int a;   // { dg-error "'static' specifier conflicts with 'typedef'" }
typedef register int b; // { dg-error "'register' specifier conflicts with 'typedef'" }
typedef extern int c;   // { dg-error "'extern' specifier conflicts with 'typedef'" }
static typedef int a;   // { dg-error "'typedef' specifier conflicts with 'static'" }

void foo()
{
  typedef auto int bar; // { dg-error "'auto' specifier conflicts with 'typedef'|two or more data types" }
}
