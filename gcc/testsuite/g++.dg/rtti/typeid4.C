// { dg-do run }
// { dg-options "-O2" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <typeinfo>
#include <iostream>

struct A { virtual ~A () {} };

struct APtr
{ 
  APtr (A* p)  : p_ (p) { }
  A& operator* () const { return *p_; }
  A* p_;
};

int main ()
{ 
  APtr ap (new A);
  std::type_info const* const exp = &typeid (*ap);
  for (bool cont = true; cont; cont = false)
    { 
      std::cout << "inner: cont " << cont << std::endl;
      if (exp) ;
    }
}

