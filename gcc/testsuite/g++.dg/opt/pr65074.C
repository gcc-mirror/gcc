// PR middle-end/65074
// { dg-do link { target pie } }
// { dg-options "-pie -fpie -O2" }
// { dg-skip-if "requires hosted libstdc++ for fstream" { ! hostedlib } }

#include <fstream>

using namespace std;

__attribute__((noinline, noclone)) void
foo (const char *fname)
{
  ifstream f (fname);
}

int
main ()
{
  foo ("foobar");
}
