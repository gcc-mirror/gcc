// PR c++/43031
// { dg-do compile { target i?86-*-* x86_64-*-* } }

class T;
class L { };
class P : public L
{
  typedef void (__attribute__((__stdcall__)) T::*F) (L*);
  void f(bool aAdd);
};
class T
{
public:
    virtual void __attribute__((__stdcall__)) A(L *listener) = 0;
    virtual void __attribute__((__stdcall__)) R(L *listener) = 0;
};
void P::f(bool aAdd)
{
  F addRemoveEventListener = (aAdd ? &T::A : &T::R);
}
