// PR c++/127207
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodule-header" }

template <typename T>
class Foo
{
    friend auto createFoo<T>() -> Foo;

    Foo(){}
};

template <typename T>
auto createFoo() -> Foo<T>
{
    return Foo<T>{};
}
