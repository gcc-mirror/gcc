// PR c++/58650

struct A
{
  friend int i = 0;  // { dg-error "14:.i. is neither function nor member function; cannot be declared friend" }
};
