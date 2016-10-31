/* { dg-do compile } */
/* { dg-options -Wshadow=compatible-local } */

class Bar {
};

class ChildBar : public Bar {
};

Bar bar;

class Foo {
 private:
  int val;

 public:
  int func1(int x) {
    int val;
    val = x;
    return val;
  }

  int func2(int i) { // { dg-message "note: shadowed declaration is here" }
    int a = 3;       // { dg-message "note: shadowed declaration is here" }

    for (int i = 0; i < 5; ++i) {   // { dg-warning "shadows a parameter" }
      for (int i = 0; i < 3; ++i) { // { dg-warning "shadows a previous local" }
        int a = i;   // { dg-warning "shadows a previous local" }
        func1(a);
      }
    }

    return a;
  }

  int func3() {
    int bar;
    float func1 = 0.3;
    int f = 5;       // { dg-message "note: shadowed declaration is here" }

    if (func1 > 1) {
      float f = 2.0; // { dg-warning "shadows a previous local" }
      bar = f;
    }
    else
      bar = 1;
    return bar;
  }

  void func4() {
    Bar *bar;        // { dg-bogus "shadowed declaration" }
    ChildBar *cbp;   // { dg-bogus "shadowed declaration" }
    Bar *bp;         // { dg-message "note: shadowed declaration is here" }
    if (val) {
      int bar;       // { dg-bogus "shadows a previous local" }
      Bar *cbp;      // { dg-bogus "shadows a previous local" }
      ChildBar *bp;  // { dg-warning "shadows a previous local" }
      func1(bar);
    }
  }
};

// { dg-message "note: shadowed declaration" "" { target *-*-* } 26 }
