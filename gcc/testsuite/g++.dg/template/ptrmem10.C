// PR c++/15096

template <typename T_>
class C1
{
public:
    C1 ();
    ~C1 ();
    const int C1<T_>::* getPtr () const;

private:
    int x;
    T_ y;
};


template <typename T_>
const int C1<T_>::* C1<T_>::getPtr () const
{ return &C1<T_>::x; }


