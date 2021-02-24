/* PR c++/99074 - gcc 8 and above is crashing with dynamic_cast<>() on null
   pointer with optimization level -O1 and above
   { dg-do run }
   { dg-options "-O1 -Wall" } */

class Base
{
public:
  virtual ~Base() {}
  virtual void op() = 0;
};

class Object: public virtual Base { };

class AbstractBase: public virtual Base
{
public:
  Object* _to_object ()
  {
    return dynamic_cast<Object*>(this);   // { dg-warning "\\\[-Wnonnull" "" { xfail *-*-* } }
  }
};

class MyAbstractClass: public virtual AbstractBase
{
public:
  static MyAbstractClass* _nil () { return 0; }
};


int main ()
{
  MyAbstractClass *my_abs_type = MyAbstractClass::_nil ();
  AbstractBase *abs_base = my_abs_type;
  Object *obj = abs_base->_to_object ();

  __builtin_printf ("object is: %p\n", obj);

  return 0;
}
