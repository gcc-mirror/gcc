// { dg-do assemble  }
// GROUPS passed nested-classes
int x;
class enclose {
public:
  int x;			// { dg-message "" }

  class inner {
  public:
    void f (int i) {
      x = i;// { dg-error "non-static" } .*
    }
  };
};

