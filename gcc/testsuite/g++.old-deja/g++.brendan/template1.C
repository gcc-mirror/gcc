// { dg-do assemble  }
// GROUPS passed templates
// g++-2.2.1: member functions returning a template type 


template <class T> struct list { };

struct A
{
  list<int> L;

  list<int>  get_list();

};


list<int> A::get_list() { return L; }
