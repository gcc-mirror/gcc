// { dg-do assemble  }
// Testcase from Alexander Zvyagin <zvyagin@mx.ihep.su>
// Check implicit conversion from string constants into typedefs

typedef char CHAR;
void f2(CHAR *s="");

