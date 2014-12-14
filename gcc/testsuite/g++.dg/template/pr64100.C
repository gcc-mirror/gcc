// { dg-do compile { target c++11 } }

template<typename> struct foo // { dg-message "note" }
{ // { dg-error "incomplete type" }
    static_assert(noexcept(((foo *)1)->~foo()), "");
}; 

template class foo<int>;

