// PR c++/64994
// { dg-do compile { target c++11 } }

class TimeStamp {
public:
  constexpr TimeStamp() : mValue() {}
  int mValue;
};

class A {
  class B;
  A(bool);
};
class C {
  TimeStamp mFadeBeginTime;
};
class A::B {
public:
  B(int) {}
  TimeStamp mPrevEventTime[1];
};

A::A(bool) {
  new C;
  B(0);
}
