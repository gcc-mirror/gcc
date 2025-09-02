// Origin: PR debug/46101
// { dg-options "-gdwarf-2 -Wno-non-c-typedef-for-linkage" }
// { dg-do compile }

typedef struct
{
  virtual void f () { }
} A;

A a;

