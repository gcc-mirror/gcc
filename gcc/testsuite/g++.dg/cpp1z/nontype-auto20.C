// PR c++/104074
// { dg-do compile { target c++17 } }

template<auto> class gr_sp;
template<class T> using gr_rp = gr_sp<T::value + 42>;
