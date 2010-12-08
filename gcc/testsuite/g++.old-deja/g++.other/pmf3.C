// { dg-do assemble  }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ was crashing after giving errors.

template<class T>
  void connect_to_method( // { dg-message "connect_to_method|no known conversion" }
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
  connect_to_method(this,&show);   // { dg-error "no match" } invalid pmf expression
  // { dg-message "candidate" "candidate note" { target *-*-* } 23 }
  connect_to_method(this,&expose); // { dg-error "pointer to member" } invalid pmf expression
}
