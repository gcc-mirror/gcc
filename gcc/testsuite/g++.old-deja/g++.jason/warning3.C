// { dg-do assemble  }
// { dg-options "-Wshadow" }
// Bug: overloading of 'A' for template causes bogus shadowing warnings.

template<class T> 
class A
{
 public:
   virtual ~A() {}
};

template class A<int>;
