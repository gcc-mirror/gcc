// { dg-do assemble  }

template <class Type>
class CVector {
public:
    CVector<int> f() const
    {
       CVector<int> v();
       return v;		// { dg-error "convert" }
    }
    CVector<long> g() const
    {
       CVector<long> v();
       return v;		// { dg-error "convert" }
    }
};
