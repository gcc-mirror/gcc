template <bool> struct A {};
 
template <bool> struct B
{
  void foo()
  {
    const int i=0;
    typedef A< i<=1 > C; // { dg-error "" } 
    typedef A< i<=2 > C; // { dg-error "" } 
  }
};
