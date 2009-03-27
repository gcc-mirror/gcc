// PR c++/35652: wrong location and duplicated warning.
// { dg-do compile }
// { dg-options "-fshow-column" }
#include <string>
int foo() {
  // blank line padding, could also be code...
  //
  //
  //
  //
  //
  //
  //
  //
  //
  std::string s = "";
  s += 'x' + "y";  // { dg-warning "14:offset '120' outside bounds of constant string" }
  // { dg-bogus "offset '120' outside bounds of constant string.*offset '120' outside bounds of constant string" "duplicated" { target *-*-* } 17 }
}

int bar()
{
  const char *s = 'z' + "y"; /* { dg-warning "25:offset '122' outside bounds of constant string" } */
}

int g()
{
  char str[2];
  const char *p = str + sizeof(str);
}
