// { dg-do assemble  }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ was crashing after giving errors.

template<class T>
  void connect_to_method(
    T *receiver,
    void (T::*method)())
  {}

class Gtk_Base
{ 
public:
  void expose();
  void show();
  void show(int);
  Gtk_Base();
};


Gtk_Base::Gtk_Base()
{
  connect_to_method(this,&show);   // { dg-error "" } invalid pmf expression
  connect_to_method(this,&expose); // { dg-error "" } invalid pmf expression
}
