// { dg-do compile }
// [dcl.init] paragraph 13.
int x = { 2 };
const char * y = { "hello" };
int a = 2;
int b = { 2,3 }; // { dg-error "too many initializers" }
int c = { { 2 } } ; // { dg-error "braces around scalar initializer" }

