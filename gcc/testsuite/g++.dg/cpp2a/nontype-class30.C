// PR c++/91754 - Fix template arguments comparison with class NTTP.
// { dg-do compile { target c++2a } }

struct S {};

template<S s>
struct T {
  T();
};

template<S s>
T<s>::T() {} 

S s;
T<s> t;
