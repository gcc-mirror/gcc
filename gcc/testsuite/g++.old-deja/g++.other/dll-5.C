// { dg-do assemble { target arm-*-*pe } }
// dllimport is "sorta like" to "extern".
// set compiler_result "(\nfoo1:.*\nfoo2:|\nfoo2:.*\nfoo1:)"
// set not_compiler_result "__imp_"

__declspec (dllimport) int foo1;
int foo1;

__declspec (dllimport) int foo2;
int foo2 = 5;

int f () { return foo1 + foo2; }
