// Build don't link:
// f() should be able to access B::j, as of FDIS [class.protected]/1

// Subject: Re: [bug] Inheritance and friend access control broken
// References: <199803032141.WAA09332@piano.dptmaths.ens-cachan.fr>
// <orhg5ff544.fsf@iguacu.dcc.unicamp.br>
// <199803041125.MAA06937@cor.dptmaths.ens-cachan.fr>
// <orn2f6ek92.fsf@iguacu.dcc.unicamp.br> <19980304102900.46897@dgii.com>
// From: Alexandre Oliva <oliva@dcc.unicamp.br>
// Date: 06 Mar 1998 01:43:18 -0300

template <int*>
class X {};

template <typename T>
void g();

struct S;

template <typename T>
struct R;

class B {
protected:
  int i; // ERROR - in this context
  static int j;
};

class D : public B {
  friend void f();
  template <typename T>
  friend void g();
  friend struct S;
  template <typename T>
  friend struct R;
};

struct S {
  void h();
  X<&B::j> x;
};

template <typename T>
struct R {
  void h();
  X<&B::j> x;
};

B b;
D d;

void f()
{
    b.i = 3; // ERROR - protected
    d.i = 4;
    B::j = 5;
    D::j = 6;
}

template <typename T>
void g()
{
    b.i = 3; // ERROR - protected
    d.i = 4;
    B::j = 5;
    D::j = 6;
}

template void g<int>();

void S::h()
{
  b.i = 3; // ERROR - protected
  d.i = 4;
  B::j = 5;
  D::j = 6;
}

template <typename T>
void R<T>::h() 
{
  b.i = 3; // ERROR - protected
  d.i = 4;
  B::j = 5;
  D::j = 6;
}

template struct R<double>;
