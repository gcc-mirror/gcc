// Build don't link:

class C {
public:
  virtual void f();
};

extern volatile C* cp;
extern volatile C& cr;

void f ()
{
  dynamic_cast<void*>(cp); // ERROR - cannot dynamic_cast
  dynamic_cast<C&>(cr); // ERROR - cannot dynamic_cast
}
