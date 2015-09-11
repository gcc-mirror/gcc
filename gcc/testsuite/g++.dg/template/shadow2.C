template <class A, // { dg-message "template parameter 'A'" }
          template <class A> class B> // { dg-error "declaration of template parameter 'A' shadows" }
class X;
