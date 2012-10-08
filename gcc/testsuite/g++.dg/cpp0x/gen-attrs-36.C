// PR c++/43031
// { dg-options "-std=c++11 -pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }

class T;
class L { };
class P : public L
{
  typedef void (T::* [[gnu::__stdcall__]] F2) (L*); // { dg-warning "only applies to function types" }
  typedef void (T::*F) (L*) [[gnu::__stdcall__]];
  void f(bool aAdd);
};

class T
{
public:
  virtual void  A(L *listener) [[gnu::__stdcall__]] = 0;
  virtual void R(L *listener)  [[gnu::__stdcall__]] = 0;
};
void P::f(bool aAdd)
{
  F addRemoveEventListener = (aAdd ? &T::A : &T::R);
}
