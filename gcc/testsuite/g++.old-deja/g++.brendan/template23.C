// { dg-do assemble  }
// GROUPS passed templates
template <class T>
class T_Base
{};

template <class T>
class T_Derived
: public T_Base<T>
{};


class Base
{
public:
  Base (float name);
};

class Derived
: public Base
{
private:
  T_Base<int>*	data_;	// Fix (1): Change date_ from T_Base<int>* to T_Derived<int>*
  
public:
  Derived (float name); 
};


Derived::
Derived (float name)
:  Base(name),
   data_(new T_Derived<int>()) 
{}
