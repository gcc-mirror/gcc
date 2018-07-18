// PR c++/82299
// { dg-do compile { target c++11 } }
// { dg-options "-Wuseless-cast" }

enum Enum : char { A = 0, B = 1 };

struct S {
  Enum e { Enum::A };	// { dg-bogus "useless cast to type" }
};
