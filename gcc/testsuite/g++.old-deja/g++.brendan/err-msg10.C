// { dg-do assemble  }
// GROUPS passed error-messages
class foo {
public:
  virtual static int f () = 0;// { dg-error "" }  member `f' cannot be declared both virtual and static.*
};
