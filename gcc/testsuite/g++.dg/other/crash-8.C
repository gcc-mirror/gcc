// Origin: PR c++/42797
// { dg-options "-g -O2 -std=c++0x" }

template<typename _Tp, _Tp __v>     struct integral_constant     {
    static const _Tp value = __v;
};

template<typename _Tp>     _Tp declval();

template<typename _Tp, typename... _Args>
class __is_constructible_helper  {
};

template<typename _Tp, typename _Arg>
class __is_constructible_helper<_Tp, _Arg>  {

    template<typename _Tp1, typename _Arg1>
    static decltype(static_cast<_Tp1>(declval<_Arg1>()), char())  __test(int);
public:
    static const bool __value = sizeof(__test<_Tp, _Arg>(0)) == 1;
};

template<typename _Tp, typename... _Args>
struct is_constructible     : public integral_constant<bool,__is_constructible_helper<_Tp, _Args...>::__value>     { };

template<bool, typename _Tp = void>
struct enable_if  { };

template<typename _Tp>
struct enable_if<true, _Tp>     {
    typedef _Tp type;
};

template<class _T1, class _T2>     struct pair     {
    _T1 first;
    _T2 second;

    template<class _U2, class = typename  enable_if<is_constructible<_T2, _U2&&>::value>::type>
    pair(const _T1& __x, _U2&& __y)  : first(__x),
                                       second(__y) { }
};

namespace __gnu_cxx {
template<typename _Tp>
class new_allocator     {
public:
    new_allocator() throw() { }
    new_allocator(const new_allocator&) throw() { }
};
}

template<typename _Tp>
class allocator: public __gnu_cxx::new_allocator<_Tp>     {
public:

    template<typename _Tp1>
    struct rebind  {
        typedef allocator<_Tp1> other;
    };
};


template<typename _Tp, typename _Alloc>     struct _Vector_base     {
    typedef typename _Alloc::template rebind<_Tp>::other _Tp_alloc_type;

    struct _Vector_impl       : public _Tp_alloc_type   {
        _Vector_impl()
        { }
    };
public:

    _Vector_impl _M_impl;
};

template<typename _Tp, typename _Alloc = allocator<_Tp> >
class vector : protected _Vector_base<_Tp, _Alloc> {
    typedef _Alloc allocator_type;
public:
    vector()       { }
    explicit       vector(int, const allocator_type& __a = allocator_type())
    {
    }
};


template <typename _Key, typename _Tp>
class map {
    typedef _Key key_type;
    typedef _Tp mapped_type;
    typedef pair<const _Key, _Tp> value_type;
public:

    void insert(const value_type& __x)
    {
    }

    mapped_type&       operator[](const key_type& __k)       {
        insert(value_type(__k, mapped_type()));
    }

};

struct Foo {
    Foo() {}      template<typename Tp>     Foo(Tp *p) {} };
void foo() {
    map <int, vector<Foo>> the_map;
    the_map[1] = vector<Foo>();
}

