// Build don't link: 
// GROUPS passed error-messages
class foo {
public:
  int ~foo ();// ERROR -  return type specification for destructor invalid.*
};
