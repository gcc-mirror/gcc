// PR c++/7503
// { dg-do compile }
// { dg-options "-O2 -Wno-deprecated" }

extern int A, B;

void test1()
{
  (A++ <? B) = 0;  // { dg-error "lvalue required" }
}

void test2()
{
  (A <? B++) = 0;  // { dg-error "lvalue required" }
}

void test3()
{
  (A++ >? B) = 0;  // { dg-error "lvalue required" }
}

void test4()
{
  (A >? B++) = 0;  // { dg-error "lvalue required" }
}

