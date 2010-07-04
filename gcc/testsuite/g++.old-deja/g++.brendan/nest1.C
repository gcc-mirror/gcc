// { dg-do assemble  }
// GROUPS passed nested-classes
int x;
class enclose {
public:
  int x;			// { dg-error "non-static" }

  class inner {
  public:
    void f (int i) {
      x = i;// { dg-error "" } .*
    }
  };
};

