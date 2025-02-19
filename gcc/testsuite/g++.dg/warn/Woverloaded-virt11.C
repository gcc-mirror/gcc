// PR c++/109918 - More tests with multiple inheritance
// { dg-do compile { target c++11 } }
// { dg-additional-options -Woverloaded-virtual }

struct Troops { virtual ~Troops(); };
struct Control {
  virtual int GetControl() const { return 42; } // { dg-warning "was hidden" }
};
struct Army : Troops, Control {
  template<class T> void GetControl() const; // { dg-message "by" }
};


struct A {
  virtual void get() const;
};
struct B {
  virtual void get() const;
  virtual void get(char) const; // { dg-warning "was hidden" }
};

struct C : A, B {
  virtual void get() const; // { dg-message "by" }
  virtual void get(int) const;
};
