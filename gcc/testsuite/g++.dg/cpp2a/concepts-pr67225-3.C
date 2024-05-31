// PR c++/67225
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts" }

template <class>
concept Dummy = true;

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
