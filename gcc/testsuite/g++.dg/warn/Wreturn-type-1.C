// PR c++/11725
// { dg-options "-Wreturn-type" }

template <class T>
struct A 
{
  int foo()
  {
    throw "Stop";
  }
};
