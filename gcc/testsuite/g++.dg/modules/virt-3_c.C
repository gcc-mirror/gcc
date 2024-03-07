// PR c++/114229
template<class> struct basic_streambuf { virtual void overflow() { } };
template struct basic_streambuf<long>;
