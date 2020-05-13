// PR c++/67225
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-ts" }

template <class>
concept bool Dummy = true;

template <typename>
class example {
    template <Dummy U>
    friend auto func();
};

class test {
    test() = default;
};

int main()
{
    test t; // { dg-error "private within this context" }
}
