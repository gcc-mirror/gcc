// Build don't link: 
// GROUPS passed error-messages
class foo {
public:
  friend mutable int x ();// ERROR -  non-object member `x' cannot be declared `mutable'
};
