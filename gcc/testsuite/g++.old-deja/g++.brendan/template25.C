// { dg-do assemble  }
// GROUPS passed templates
template <class T>
class Temp_Base
{};

template <class T>
class Temp_Derived
: public Temp_Base<T>
{
public:
  Temp_Derived (int i = 10) {}
};


class Base
{
  float r_;
public:
  Base (float r) : r_(r) {}
};

class Derived
: public Base
{
private:
  Temp_Base<int>*  boo_;	   
public:
  Derived (float);
};

Derived::
Derived (float form)
: Base(form),
  boo_(new Temp_Derived<int>)
{}
