// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S {
  bool operator! (int, ...); // { dg-error "" } 
};
