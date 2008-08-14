// PR c++/34485

struct A
{
  template<T::X> struct X; // { dg-error "error: 'T' has not been declared|error: declaration of 'template<int X> struct A::X'|error:  shadows template parm 'int X'" }
};
