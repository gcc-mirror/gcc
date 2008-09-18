// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR108: Are classes nested in templates dependent?

template <class T> struct S {
  struct I1 {
    typedef int X;
  };
  struct I2 : public I1 {
    X x;    // { dg-error "does not name a type" "name" }
	    // { dg-message "note" "note" { target *-*-* } 10 }
  };
};
