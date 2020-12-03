// PR 98115, dependent array types lead to specialization issues

template <class> class Stringify;
template <long N> class Stringify<const char[N]>;
