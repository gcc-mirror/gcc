// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/13495: Nested class as template friend.

template<typename T>
class A{
public:
    class B
    {
        void func1(void);
        void func2(void);
    };
};

template<typename Q>
class F1
{
    friend class A<Q>::B;
    enum { foo = 0 };	// { dg-error "private" }
};

template<typename Q>
class F2
{
    template<typename T>
    friend class A<T>::B;
    enum { foo = 0 };
};

template <typename T>
void A<T>::B::func1(void)
{
    (void)F1<T>::foo;
    (void)F2<T>::foo;
}

template <typename T>
void A<T>::B::func2(void)
{
    (void)F1<T*>::foo;	// { dg-error "context" }
    (void)F2<T*>::foo;
}

template class A<int>;	// { dg-message "instantiated" }
