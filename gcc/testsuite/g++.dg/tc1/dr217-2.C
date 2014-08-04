// { dg-do compile }
// DR217: Default arguments for non-template member functions of class 
//  templates 

template <class T>
struct S
{
  static void foo (int);
};

template <class T>
void S<T>::foo (int = 0)  // { dg-error "" "default arguments for parameters of member functions of class templates can be specified in the initial declaration only" }
{ }
