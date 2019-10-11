// { dg-do compile { target c++2a } }

template <class>
concept Dummy = true;

template <typename>
class example {
    template <Dummy<> U>
    friend auto func();
};
