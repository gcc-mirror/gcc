// Build don't link: 
// GROUPS passed old-abort
struct B {
    B();
};
 
class C : virtual public B
{
  public:
    C() { }
};
