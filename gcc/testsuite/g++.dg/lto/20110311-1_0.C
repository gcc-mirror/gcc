/* { dg-lto-do link } */
/* { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" } */

struct NullType {};

template <class T, class U>
struct TList
{
    typedef T Head;
    typedef U Tail;
};

template <class T>
struct TListLength {};

template <class T, class U>
struct TListLength<TList<T,U> >
{
    enum
    {
        Ret = 1 + TListLength<U>::Ret
    };
};

template <>
struct TListLength<NullType>
{
    enum
    {
        Ret = 0
    };
};

template <class Moves>
class DDQMC
{
public:
    int* moves[TListLength<Moves>::Ret];
    inline DDQMC();
private:
};

template <class Moves>
DDQMC<Moves>::DDQMC()
{
}

int main()
{
    typedef DDQMC< TList<float, TList<int, NullType> > > mytype;
}
