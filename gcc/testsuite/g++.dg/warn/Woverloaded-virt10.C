// PR c++/117114 - Test case from PR c++/117114
// { dg-do compile { target c++11 } }
// { dg-additional-options -Woverloaded-virtual }

struct Troops { virtual ~Troops(); };
struct Control { virtual int GetControl() const = 0; };
struct Army : Troops, Control { int GetControl() const override; };

struct VirtualControl : virtual Control {
  int GetControl() const override;
};
