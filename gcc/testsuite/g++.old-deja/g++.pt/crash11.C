// { dg-do assemble  }

class A
{
      class A_impl;
   public: 
      A(){}
};


template <class j> class A::A_impl // { dg-error "does not declare a template" }
{
};
