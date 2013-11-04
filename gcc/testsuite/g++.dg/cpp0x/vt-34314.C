// { dg-options "-std=c++11" }

template<typename Fun, typename... Args> // { dg-error "template parameter" }
struct call;

template<typename Fun, typename Arg0>
struct call // { dg-error "redeclared here" }
{
    template<typename Sig>
    struct result;

    template<typename X, typename Y>
    struct result<X(Y)>
    {
        typedef X type;
    };
};


template<typename Fun, int... N> // { dg-error "template parameter" }
struct call2;

template<typename Fun, int N>
struct call2 // { dg-error "redeclared here" }
{
    template<typename Sig>
    struct result;

    template<typename X, typename Y>
    struct result<X(Y)>
    {
        typedef X type;
    };
};

template<typename Fun, template<typename> class... TT> // { dg-error "template parameter" }
struct call3;

template<typename Fun, template<typename> class TT>
struct call3 // { dg-error "redeclared here" }
{
    template<typename Sig>
    struct result;

    template<typename X, typename Y>
    struct result<X(Y)>
    {
        typedef X type;
    };
};
