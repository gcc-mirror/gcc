/* Test C11 _Generic.  Test we follow the resolution of DR#423.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

char const *a = _Generic ("bla", char *: "");
char const *b = _Generic ("bla", char[4]: ""); /* { dg-error "not compatible with any association" } */
char const *c = _Generic ((int const) { 0 }, int: "");
char const *d = _Generic ((int const) { 0 }, int const: ""); /* { dg-error "not compatible with any association" } */
char const *e = _Generic (+(int const) { 0 }, int: "");
char const *f = _Generic (+(int const) { 0 }, int const: ""); /* { dg-error "not compatible with any association" } */
