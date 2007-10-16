//PR c++/28736

template<void> struct A                 // { dg-error "not a valid type" }
{
    template<typename> friend struct B;
};

template<typename> struct B {};

B<int> b;
