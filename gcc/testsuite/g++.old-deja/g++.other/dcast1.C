// { dg-do assemble  }

class C {
public:
  virtual void f();
};

extern volatile C* cp;
extern volatile C& cr;

void f ()
{
  dynamic_cast<void*>(cp); // { dg-error "" } cannot dynamic_cast
  dynamic_cast<C&>(cr); // { dg-error "" } cannot dynamic_cast
}
