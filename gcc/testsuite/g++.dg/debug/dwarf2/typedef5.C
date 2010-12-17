// Origin: PR debug/46101
// { dg-options "-g -feliminate-dwarf2-dups" }
// { dg-do compile }

typedef struct
{
  virtual void f () { }
} A;

A a;
