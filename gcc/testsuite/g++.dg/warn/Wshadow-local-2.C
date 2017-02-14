/* { dg-do compile } */
/* { dg-options -Wshadow=local } */

class Bar {
};

class ChildBar : public Bar {
};

Bar bar;             // { dg-bogus "shadowed declaration" }

class Foo {
 private:
  int val;

 public:
  int func1(int x) {
    int val;         // { dg-bogus "shadows a member" }
    val = x;
    return val;
  }

  int func2(int i) { // { dg-message "shadowed declaration is here" }
    int a = 3;       // { dg-message "shadowed declaration is here" }

    for (int i = 0; i < 5; ++i) {   // { dg-warning "shadows a parameter" }
      for (int i = 0; i < 3; ++i) { // { dg-warning "shadows a previous local" }
        int a = i;   // { dg-warning "shadows a previous local" }
        func1(a);
      }
    }

    return a;
  }

  int func3() {
    int bar;         // { dg-bogus "shadows a global" }
    float func1 = 0.3; // { dg-bogus "shadows a member" }
    int f = 5;       // { dg-message "shadowed declaration is here" }

    if (func1 > 1) {
      float f = 2.0; // { dg-warning "shadows a previous local" }
      bar = f;
    }
    else
      bar = 1;
    return bar;
  }

  void func4() {
    Bar *bar;        // { dg-message "shadowed declaration is here" }
    ChildBar *cbp;   // { dg-message "shadowed declaration is here" }
    Bar *bp;         // { dg-message "shadowed declaration is here" }
    if (val) {
      int bar;       // { dg-warning "shadows a previous local" }
      Bar *cbp;      // { dg-warning "shadows a previous local" }
      ChildBar *bp;  // { dg-warning "shadows a previous local" }
      func1(bar);
    }
  }
};

// { dg-message "shadowed declaration is here" "" { target *-*-* } 26 }
