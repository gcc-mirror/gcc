// { dg-do compile }

// Origin: rmerkert@alphatech.com
//         Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/12495: ICE looking up class template in local class.

template <typename> struct A {};

template <typename T> void foo()
{
    struct B
    {
        B (const A<T>&);
    };
}
