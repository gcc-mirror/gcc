// { dg-do compile }
// { dg-options "-Weffc++" }
// Contributed by Benjamin Kosnik <bkoz at redhat dot com>
// PR c++/16165 and PR c++/8211: Improve item 11 of -Weffc++


// We should not warn for this class since this kind of pointers can
//  never hold dynamic memory.
struct A {
  void (*func1)(void);
  void (A::*func2)(void);
  int A::*func3;

  int a;
  void b(void);

  A();
  ~A();
};

// We do not warn for this class because there is no destructor, so we
//  assume there is no dynamic memory allocated (it could point to a
//  global variable).
struct B {
  int *ptr;
  B();
};


// We should emit a warning for these
struct C1 {		// { dg-warning "" "" }
  int *ptr;
  C1();
  ~C1();
};

struct C2 {		// { dg-warning "" "" }
  int *ptr;
  C2();
  C2(const C2&);
  ~C2();
};

struct C3 {		// { dg-warning "" "" }
  int *ptr;
  C3();
  ~C3();
  C3& operator=(const C3&);
};

// But not for this
struct C4 {
  int *ptr;
  C4();
  C4(const C4&);
  ~C4();
  C4& operator=(const C4&);
};
