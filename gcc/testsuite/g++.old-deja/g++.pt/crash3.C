// Build don't link:

template <class Type>
class CVector {
public:
    CVector<int> f() const
    {
       CVector<int> v();
       return v;
    }
    CVector<long> g() const
    {
       CVector<long> v();
       return v;
    }
};
