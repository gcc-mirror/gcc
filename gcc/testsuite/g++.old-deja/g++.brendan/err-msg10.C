// Build don't link: 
// GROUPS passed error-messages
class foo {
public:
  virtual static int f () = 0;// ERROR -  member `f' cannot be declared both virtual and static.*
};
