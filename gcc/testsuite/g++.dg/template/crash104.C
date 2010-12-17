// PR c++/46046

template <class T>
    struct foo
{
    template <class U, class V = void>
        struct type
    {};

    template <class V>
        struct type<
            typename T::template some_type<int>,
            V
        >
    {};
};

template <class T>
    class bar
{};

int main()
{
  typedef foo<bar<int> > cont;
  cont::type<char> obj; // { dg-error "cannot be defined" }
}
