// PR c++/65051

template<typename T> struct wrap { typedef T type; };
template <class T> class rv: public wrap <T>::type {};

template <class value_type>
struct circular_buffer
{
    typedef const value_type& param_value_type;
    typedef rv< value_type >& rvalue_type;

    void push_back(param_value_type item) {}
    void push_back(rvalue_type item) {}
};

union U { int i; char c; };

void f(circular_buffer<U> b, const U& u) { b.push_back(u); }
