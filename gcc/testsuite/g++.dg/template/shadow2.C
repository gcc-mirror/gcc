template <class A, // { dg-error "shadows template parm 'class A'" }
          template <class A> class B> // { dg-error "declaration of 'class A'" }
class X;
