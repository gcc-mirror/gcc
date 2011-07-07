// PR c++/49663
// { dg-options -std=c++0x }

struct Nosm
{
    int m_R;
};

namespace dx {

    struct onc
    {
        typedef void(*Cb)();

        onc(Cb cb);
    };

    struct grac
    {
        template<class Derived> static void once();
    };

    template<class Derived>
        struct tonc : onc
        {
            tonc() : onc(&grac::once<Derived>) {}

            static Derived& get();
        };

    template<class Derived> void grac::once()
    {
        tonc<Derived>::get().h();
    }
}

namespace
{
    template<typename T, int = sizeof(&T::m_R)>
        struct has_R { };

    template<typename T>
        inline void
        setR(T* m, has_R<T>* = 0)
        { }

    inline void setR(...) { }
}

template<typename M>
    struct Qmi
    : dx::tonc<Qmi<M> >
    {
        void h()
        {
            setR(&msg);
        }

        M msg;
    };

Qmi<Nosm> x;
