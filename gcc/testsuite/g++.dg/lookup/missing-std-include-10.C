// { dg-do compile }
// { dg-additional-options -fdiagnostics-show-caret }
// comment






// Intentional blank lines








#include "missing-std-include-10.h"
// HERE






// Intentional blank lines









int main ()
{
  return strcmp ("", "");
}
// { dg-additional-files "missing-std-include-10.h" }
// { dg-regexp {[^\n]*: error: 'strcmp' was not declared in this scope\n *return strcmp [^\n]*;\n *\^~*\n} }
// { dg-regexp {[^\n]* note: 'strcmp' is defined in header[^\n]*\n #include "missing-std-include-10.h"\n\+#include <cstring>\n // HERE\n} }
