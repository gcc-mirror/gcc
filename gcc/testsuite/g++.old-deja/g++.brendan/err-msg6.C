// { dg-do assemble  }
// GROUPS passed error-messages
class foo {
public:
  ~bar () {}// { dg-error "" }  destructor `bar' must match class name `foo'.*
};

