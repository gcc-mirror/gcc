// { dg-do compile }
// { dg-options "-Weffc++" }
// Contributed by Benjamin Kosnik <bkoz at redhat dot com>
// PR c++/16169 : Improve -Weffc++ rule 15

struct A {
 const A& foo();
 const A& operator=(int)
 { return foo(); }
};

struct B {
  B& foo();
  B& operator=(int)
  { return foo(); }
};

struct C {
  C& operator=(int)
  { return *this; }
};

struct D {
  D operator=(int)
  { return *this; }      // { dg-warning "should return a reference" }
};

struct E {
  E& foo();
  E operator=(int)
  { return foo(); }      // { dg-warning "should return a reference" }
};

struct F
{
  operator float();
  float operator=(int)
  { return *this; }      // { dg-warning "should return a reference" }
};
