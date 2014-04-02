// PR c++/43031
// { dg-options "-pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-require-effective-target c++11 }

// c++11 attributes that apply to types are ignored for now

class T;
class L { };
class P : public L
{
  typedef void (T::* [[gnu::__stdcall__]] F2) (L*); // { dg-warning "ignored" }
  typedef void (T::*F) (L*) [[gnu::__stdcall__]]; // { dg-warning "ignored" }
  void f(bool aAdd);
};

class T
{
public:
  virtual void  A(L *listener) [[gnu::__stdcall__]] = 0; // { dg-warning "ignored" }
  virtual void R(L *listener)  [[gnu::__stdcall__]] = 0; // { dg-warning "ignored" }
};
void P::f(bool aAdd)
{
  F addRemoveEventListener = (aAdd ? &T::A : &T::R);
}
