// { dg-do assemble  }
// GROUPS passed gb scope
class enclose {
  int e;
protected:
  class nested {
    int n;
  };
};

class derived : public enclose {
protected:
  class nested_derived : public nested {
    int nd;
  };
};
