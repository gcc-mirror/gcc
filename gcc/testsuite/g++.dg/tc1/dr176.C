// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR176: Name injection and templates 

namespace N1 {
  template <class T> struct Base {
    Base* p;
    Base<T*>* p2;
    ::Base* p3;    // { dg-error "" "" }
  };

  template <class T> struct Derived: public Base<T> {
    Base* p;     // { dg-error "" "unqualified name binds to N1::Base" }
    Base<T*>* p2;
    typename Derived::Base* p3;   // { dg-bogus "" "injected class name in derived classes" }
  };

  template struct Derived<void>;  // { dg-bogus "required from here" "everything should be looked up at parsing time (after DR224)" }
}


namespace N2 {
  template <class T> struct Base {};
  template <class T> struct Derived: public Base<T> {
    typename Derived::template Base<double>* p1;  // { dg-bogus "" "" }
  };

  template struct Derived<void>;
}
