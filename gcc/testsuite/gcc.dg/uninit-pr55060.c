/* PR tree-optimization/55060 - False un-initialized variable warnings
   { dg-do compile }
   { dg-options "-O1 -Wuninitialized" } */

static void a(int *i) { }
static void b(int p) { }
int foo(void) {
  int i;
  a(&i);
  b(i);             // { dg-bogus "\\\[-Wuninitialized" }
  return 0;
}

static void c(int *i) { }
extern void d(int p);
int bar(void) {
  int i;
  c(&i);
  d(i);             // { dg-warning "\\\[-Wuninitialized" }
  return 0;
}

extern void e(int *i);
static void f(int p) {};
int baz(void) {
  int i;
  e(&i);
  f(i);             // { dg-bogus "\\\[-Wuninitialized" }
  return 0;
}
