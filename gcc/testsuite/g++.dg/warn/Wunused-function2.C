// PR debug/66869
// { dg-do compile }
// { dg-options "-Wunused-function" }

static void test (void); // { dg-warning "'void test..' declared 'static' but never defined" }
int i;
