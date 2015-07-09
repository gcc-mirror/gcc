// Origin: PR debug/46101
// { dg-options "-gdwarf-2 -feliminate-dwarf2-dups" }
// { dg-do compile }

typedef struct
{
  virtual void f () { }
} A;

A a;

/* { dg-bogus "-feliminate-dwarf2-dups is broken for C\\+\\+, ignoring" "broken -feliminate-dwarf2-dups" { xfail *-*-* } 1 } */
