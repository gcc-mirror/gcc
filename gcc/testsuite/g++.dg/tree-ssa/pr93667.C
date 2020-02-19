// { dg-do compile }
// { dg-options "-O2 -std=c++2a" } */

struct a {};
struct b { [[no_unique_address]] a aq; };
struct c {
  int d;
  [[no_unique_address]] b e;
};
c f() {return {};}
void g() { f(); }
