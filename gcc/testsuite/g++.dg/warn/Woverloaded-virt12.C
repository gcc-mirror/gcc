// PR c++/109918 - Test covariant return types
// { dg-do compile { target c++11 } }
// { dg-additional-options -Woverloaded-virtual }

struct Big { virtual ~Big () {} };
struct Smaller : Big {};

// Single inheritance case
struct Foo {
  virtual Big* getMe() const;
};
struct Bar : Foo {
  virtual Smaller* getMe() const;
};

// Multiple inheritance case
struct Troops { virtual ~Troops(); };
struct Control {
  virtual Big* GetControl() const;
};
struct Army : Troops, Control {
  virtual Smaller* GetControl() const;
};
