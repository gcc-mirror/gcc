// PR c++/64514
// { dg-do compile { target c++11 } }

template<typename... T>
struct Functor
{
    template <T...>
    struct Inner
    {};
};

template struct Functor<>::Inner<>;

int main()
{

}
