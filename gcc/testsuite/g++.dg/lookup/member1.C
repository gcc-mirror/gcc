// PR c++/14186

struct Base 
{ 
  enum { Derived }; 
}; 
 
class Derived : public Base 
{ 
  Derived(); 
};
