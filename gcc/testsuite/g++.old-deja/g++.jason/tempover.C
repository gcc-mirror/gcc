// { dg-do assemble  }
// GROUPS passed templates overloading
#define TEMPL template <class T>
 
class B {};
 
TEMPL class A : virtual public B {
  public:
    A(int);
};
 
TEMPL A<T>::A(int){}
 
A<double> a(1);
