// Build don't link:

class A
{
      class A_impl;
   public: 
      A(){}
};


template <class j> class A::A_impl 
{ // ERROR - does not declare a template
};
