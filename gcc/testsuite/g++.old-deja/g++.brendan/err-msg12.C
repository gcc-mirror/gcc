// { dg-do assemble  }
// GROUPS passed error-messages
class foo {
public:
  friend mutable int x ();// { dg-error "" }  non-object member `x' cannot be declared `mutable'
};
