// { dg-do assemble  }
// GROUPS passed error-messages
class foo {
public:
  int ~foo ();// { dg-error "" }  return type specification for destructor invalid.*
};
