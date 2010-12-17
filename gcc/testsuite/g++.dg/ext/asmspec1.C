// PR c++/28343
// { dg-do compile }

struct A
{
  int i __asm__(int);         // { dg-error "expected" }
  static int j __asm__(int);  // { dg-error "expected" }
};
