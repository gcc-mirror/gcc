// { dg-do compile { target c++11 } }

template<typename> struct foo // { dg-message "note" }
{
    static_assert(noexcept(((foo *)1)->~foo()), ""); // { dg-error "incomplete type" }
}; 

template class foo<int>;

