// PR c++/33035

template<class A> 
struct a {
        template<class B> 
        struct b {
                template<class C>
                void f()
                {
                        struct g
                        {
                                ~g() {}
                        };
                }
        };
};
