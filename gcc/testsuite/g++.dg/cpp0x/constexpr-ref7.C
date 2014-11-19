// PR c++/57654
// { dg-do compile { target c++11 } }

int i;

constexpr int & iref = i;
constexpr int & irefref = iref;

class A {
  static constexpr int & irefref = iref;
};
