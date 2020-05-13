// { dg-do compile { target c++20 } }

template <class>
concept Dummy = true;

template <typename>
class example {
    template <Dummy<> U>
    friend auto func();
};
