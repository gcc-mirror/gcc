// Origin PR c++/48838
// { dg-do compile }

class DUChainItemSystem
{
public:

    template<class T>
    void registerTypeClass();

    static DUChainItemSystem& self();
};

template<class T>
struct DUChainItemRegistrator
{
    DUChainItemRegistrator()
    {
        DUChainItemSystem::self().registerTypeClass<T>();
    }
};
