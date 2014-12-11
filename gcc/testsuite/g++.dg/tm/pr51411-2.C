// { dg-do compile }
// { dg-options "-fgnu-tm -O0" }

namespace std {
template<typename _CharT> struct char_traits;

template<typename _Tp> class allocator {
};

template<typename _Tp> struct less {
    bool operator()(const _Tp& __x, const _Tp& __y) const {
        return __x < __y;
    }
};

template <typename _Key, typename _Compare = std::less<_Key> > class map {
public:
    _Compare _M_key_compare;
    bool find(const _Key& __x) {
        return _M_key_compare(__x, __x);
    }
};

template<typename _CharT, typename _Traits = char_traits<_CharT>, typename _Alloc = allocator<_CharT> > class basic_string {
public:
    bool compare(const basic_string& __str) const {
        return 0;
    }
    void key ();
};

typedef basic_string<char> string;

template<typename _CharT, typename _Traits>
inline bool operator<(const basic_string<_CharT, _Traits>& __lhs, const basic_string<_CharT, _Traits>& __rhs) {
    return __lhs.compare(__rhs);
}

template class basic_string<char>;

}

std::map<std::string> units;

__attribute__((transaction_callable))
void get(const std::string &name) {
    units.find(name);
}

// { dg-final { scan-assembler "_ZGTtNKSs7compareERKSs:" } }
