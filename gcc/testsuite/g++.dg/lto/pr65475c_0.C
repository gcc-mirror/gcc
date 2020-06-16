/* { dg-lto-do link } */
/* { dg-extra-ld-options { -O2 -Wno-odr -r -nostdlib } } */
/* { dg-lto-options { "-O2 -w -Wno-return-type" } } */

namespace std
{
template < class > struct char_traits;
typedef long streamsize;
template < typename, typename > class basic_streambuf;
template < typename > class A;
template < typename, typename > class basic_ostream;
template < typename _CharT, typename =
           char_traits < _CharT > >class istreambuf_iterator;
template < typename _CharT, typename =
           char_traits < _CharT > >class ostreambuf_iterator;
template < typename > class ctype;
template < typename _CharT, typename =
           istreambuf_iterator < _CharT > >class num_get;
template < typename _CharT, typename =
           ostreambuf_iterator < _CharT > >class num_put;
}
typedef int _Atomic_word;
namespace std
{
class locale
{
public:
    class facet;
private:
    class _Impl;
    _Impl *_M_impl;
};
class locale::facet
{
    _Atomic_word _M_refcount;
protected:
    virtual ~ facet ();
};
enum _Ios_Fmtflags
{   _S_boolalpha = 1, _S_dec, _S_fixed = 1 << 2, _S_hex =
        1 << 3, _S_internal = 1 << 4, _S_left = 1 << 5, _S_oct =
            1 << 6, _S_right = 1 << 7, _S_scientific = 1 << 8, _S_showbase =
                1 << 9, _S_showpoint = 1 << 10, _S_showpos = 1 << 11, _S_skipws =
                    1 << 12, _S_unitbuf = 1 << 13, _S_uppercase = 1 << 14, _S_adjustfield =
                        _S_left | _S_right | _S_internal, _S_basefield =
                            _S_dec | _S_oct | _S_hex, _S_floatfield =
                                _S_scientific | _S_fixed, _S_ios_fmtflags_end = 1 << 16
};
enum _Ios_Iostate
{   _S_goodbit, _S_badbit, _S_eofbit, _S_failbit =
        1 << 2, _S_ios_iostate_end = 1 << 16
};
class ios_base
{
    typedef _Ios_Fmtflags fmtflags;
    typedef _Ios_Iostate iostate;
    streamsize _M_precision;
    streamsize _M_width;
    fmtflags _M_flags;
    iostate _M_exception;
    iostate _M_streambuf_state;
    struct _Callback_list;
    _Callback_list *_M_callbacks;
    struct _Words
    {
        void *_M_pword;
        long _M_iword;
    } _M_word_zero;
    enum
    { _S_local_word_size = 8 };
    _Words _M_local_word[_S_local_word_size];
    int _M_word_size;
    _Words *_M_word;
    locale _M_ios_locale;
protected:
    virtual ~ ios_base ();
};
template < typename, typename > class istreambuf_iterator
{
    typedef A < char_traits < wchar_t > >istream_type;
};
template < typename, typename > class ostreambuf_iterator
{
    typedef basic_ostream < wchar_t, char_traits < wchar_t > >ostream_type;
};
template < typename, typename > class num_get:locale::facet
{
public:
    typedef istreambuf_iterator < wchar_t > iter_type;
};
template < typename, typename > class num_put:locale::facet
{
public:
    typedef ostreambuf_iterator < wchar_t > iter_type;
};
template < typename, typename > class basic_ios:ios_base
{
    typedef wchar_t char_type;
    basic_ostream < wchar_t, char_traits < wchar_t > >*_M_tie;
    char_type _M_fill;
    bool _M_fill_init;
    basic_streambuf < wchar_t, char_traits < wchar_t > >*_M_streambuf;
    ctype < wchar_t > *_M_ctype;
    num_put < wchar_t > *_M_num_put;
    num_get < wchar_t > *_M_num_get;
};
template < typename, typename > class basic_ostream:virtual basic_ios < wchar_t,
    char_traits < wchar_t >
    >
{
    typedef basic_ios __ios_type;
};
template < typename > class A:basic_ios < wchar_t, int >
{
};
class B:A < char_traits < wchar_t > >, basic_ostream < wchar_t,
    char_traits < wchar_t > >
{
};
}

class C:
    std::num_put <
    wchar_t >
{
public:
    C (int);
    iter_type
    do_put_out;
};
class
    D:
    std::num_get <
    wchar_t >
{
public:
    D (int);
    iter_type
    do_get_in;
};
template < typename > void
install_formatting_facets (std::locale, int p2)
{
    (C (p2));
}

template < typename > void
install_parsing_facets (std::locale, int p2)
{
    (D (p2));
}

std::locale a;
int b;
void
create_formatting ()
{
    install_formatting_facets < wchar_t > (a, b);
    install_parsing_facets < wchar_t > (a, b);
}
