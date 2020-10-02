// { dg-do assemble  }

template <class Type>
class CVector {
public:
    CVector<int> f() const
    {
      // local-extern :)
      CVector<int> v(); // { dg-message "old declaration" }
      // { dg-warning "empty parentheses" "" { target *-*-* } .-1 }
       return v;		// { dg-error "convert" }
    }
    CVector<long> g() const
    {
      CVector<long> v(); // { dg-error "ambiguating new" }
      // { dg-warning "empty parentheses" "" { target *-*-* } .-1 }
       return v;		// { dg-error "convert" }
    }
};
