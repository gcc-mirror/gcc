// { dg-do assemble  }
// GROUPS passed enums
class X {
  private:
    enum E1 {a1, b1}; // { dg-message "" } private
  public:
    enum E2 {a2, b2};
    };

void h(X* p) {
    X::E2 e2;
    int x2 = X::a2;

    X::E1 e1;	     // { dg-error "" } within this context
    int x1 = X::a1;  // { dg-error "" } within this context
    }
