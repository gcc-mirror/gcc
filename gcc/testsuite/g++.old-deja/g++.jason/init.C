// { dg-do assemble  }
// Bug: g++ uses an empty initializer list for its own devious purpose
// internally, and gets confused if it shows up in the input.

struct A { int i; };

A a = { };
