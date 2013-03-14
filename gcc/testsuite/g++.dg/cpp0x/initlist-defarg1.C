// PR c++/56614
// { dg-require-effective-target c++11 }

#include <initializer_list>

namespace std
{
    template<typename T>
        struct allocator
        { };

    template<typename T, typename Alloc = std::allocator<T> >
        struct vector
        {
            vector(std::initializer_list<T>, const Alloc& = Alloc()) { }
        };
}

void func() { }

enum E { ee };

struct C
{
    template<typename T>
        C(T, std::vector<E> = std::vector<E>({ ee }))
        { }
};

struct G
{
    void gen()
    {
        C c(&func);
    }
};
