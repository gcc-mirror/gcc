// PR c++/9486
// Origin: John Levon <levon@movementarian.org>
// { dg-do compile }

template <typename> struct A
{
    template <typename T> void foo(T);
};

template <typename T> void bar()
{
    A<void>().template foo<T>(0);
}

template void bar<int>();
