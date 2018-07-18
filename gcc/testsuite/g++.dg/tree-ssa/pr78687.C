/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu++14 -fdump-tree-sra" } */

#include <utility>

#define EGGS_CXX11_CONSTEXPR constexpr
#define EGGS_CXX11_STATIC_CONSTEXPR static constexpr
#define EGGS_CXX14_CONSTEXPR constexpr
#define EGGS_CXX11_NOEXCEPT noexcept

namespace eggs { namespace variants { namespace detail
{
    struct empty
    {
        EGGS_CXX11_CONSTEXPR bool operator==(empty) const { return true; }
        EGGS_CXX11_CONSTEXPR bool operator<(empty) const { return false; }
    };

    template <typename T>
    struct identity
    {
        using type = T;
    };

    template <std::size_t I>
    struct index
    {
        EGGS_CXX11_STATIC_CONSTEXPR std::size_t value = I;
    };

    template <typename ...Ts>
    struct pack
    {
        using type = pack;
        EGGS_CXX11_STATIC_CONSTEXPR std::size_t size = sizeof...(Ts);
    };

    template <typename T, T ...Vs>
    struct pack_c
    {
        using type = pack_c;
        EGGS_CXX11_STATIC_CONSTEXPR std::size_t size = sizeof...(Vs);
    };

    ///////////////////////////////////////////////////////////////////////////
    template <typename Is, bool Odd>
    struct _make_index_pack_twice;

    template <std::size_t ...Is>
    struct _make_index_pack_twice<
        pack_c<std::size_t, Is...>
      , false
    > : pack_c<std::size_t, Is..., (sizeof...(Is) + Is)...>
    {};

    template <std::size_t ...Is>
    struct _make_index_pack_twice<
        pack_c<std::size_t, Is...>
      , true
    > : pack_c<std::size_t, Is..., (sizeof...(Is) + Is)..., sizeof...(Is) * 2>
    {};

    template <std::size_t N>
    struct _make_index_pack
      : _make_index_pack_twice<
            typename _make_index_pack<N / 2>::type
          , N % 2 != 0
        >
    {};

    template <>
    struct _make_index_pack<1>
      : pack_c<std::size_t, 0>
    {};

    template <>
    struct _make_index_pack<0>
      : pack_c<std::size_t>
    {};

    template <std::size_t N>
    using make_index_pack = typename _make_index_pack<N>::type;

    template <typename Ts>
    struct _index_pack;

    template <typename ...Ts>
    struct _index_pack<pack<Ts...>>
      : _make_index_pack<sizeof...(Ts)>
    {};

    template <typename Ts>
    using index_pack = typename _index_pack<Ts>::type;

    ///////////////////////////////////////////////////////////////////////////
    template <typename Vs>
    struct all_of;

    template <bool ...Vs>
    struct all_of<pack_c<bool, Vs...>>
      : std::integral_constant<
            bool
          , std::is_same<
                pack_c<bool, Vs...>
              , pack_c<bool, (Vs || true)...> // true...
            >::value
        >
    {};

    template <typename ...Ts>
    struct all_of<pack<Ts...>>
      : all_of<pack_c<bool, (Ts::value)...>>
    {};

    template <typename ...Vs>
    struct any_of;

    template <bool ...Vs>
    struct any_of<pack_c<bool, Vs...>>
      : std::integral_constant<
            bool
          , !all_of<pack_c<bool, !Vs...>>::value
        >
    {};

    template <typename ...Ts>
    struct any_of<pack<Ts...>>
      : any_of<pack_c<bool, (Ts::value)...>>
    {};

    ///////////////////////////////////////////////////////////////////////////
    template <std::size_t I, typename T>
    struct _indexed {};

    template <typename Ts, typename Is = index_pack<Ts>>
    struct _indexer;

    template <typename ...Ts, std::size_t ...Is>
    struct _indexer<pack<Ts...>, pack_c<std::size_t, Is...>>
      : _indexed<Is, Ts>...
    {};

    empty _at_index(...);

    template <std::size_t I, typename T>
    identity<T> _at_index(_indexed<I, T> const&);

    template <std::size_t I, typename Ts>
    struct at_index
      : decltype(_at_index<I>(_indexer<Ts>{}))
    {};

    empty _index_of(...);

    template <typename T, std::size_t I>
    index<I> _index_of(_indexed<I, T> const&);

    template <typename T, typename Ts>
    struct index_of
      : decltype(_index_of<T>(_indexer<Ts>{}))
    {};
}}}

namespace eggs { namespace variants { namespace detail
{
    template <typename Ts, bool IsTriviallyDestructible>
    struct _union;

    ///////////////////////////////////////////////////////////////////////////
    template <bool IsTriviallyDestructible>
    struct _union<pack<>, IsTriviallyDestructible>
    {};

    template <typename T, typename ...Ts>
    struct _union<pack<T, Ts...>, true>
    {
        EGGS_CXX11_STATIC_CONSTEXPR std::size_t size = 1 + sizeof...(Ts);

        template <typename ...Args>
        EGGS_CXX11_CONSTEXPR _union(index<0>, Args&&... args)
          : _head(std::forward<Args>(args)...)
        {}

        template <std::size_t I, typename ...Args>
        EGGS_CXX11_CONSTEXPR _union(index<I>, Args&&... args)
          : _tail(index<I - 1>{}, std::forward<Args>(args)...)
        {}

        EGGS_CXX14_CONSTEXPR void* target() EGGS_CXX11_NOEXCEPT
        {
            return &_target;
        }

        EGGS_CXX11_CONSTEXPR void const* target() const EGGS_CXX11_NOEXCEPT
        {
            return &_target;
        }

        EGGS_CXX14_CONSTEXPR T& get(index<0>) EGGS_CXX11_NOEXCEPT
        {
            return this->_head;
        }

        EGGS_CXX11_CONSTEXPR T const& get(index<0>) const EGGS_CXX11_NOEXCEPT
        {
            return this->_head;
        }

        template <
            std::size_t I
          , typename U = typename at_index<I, pack<T, Ts...>>::type
        >
        EGGS_CXX14_CONSTEXPR U& get(index<I>) EGGS_CXX11_NOEXCEPT
        {
            return this->_tail.get(index<I - 1>{});
        }

        template <
            std::size_t I
          , typename U = typename at_index<I, pack<T, Ts...>>::type
        >
        EGGS_CXX11_CONSTEXPR U const& get(index<I>) const EGGS_CXX11_NOEXCEPT
        {
            return this->_tail.get(index<I - 1>{});
        }

    private:
        union
        {
            char _target;
            T _head;
            _union<pack<Ts...>, true> _tail;
        };
    };

    ///////////////////////////////////////////////////////////////////////////
    template <typename Ts, bool TriviallyCopyable, bool TriviallyDestructible>
    struct _storage;

    template <typename ...Ts>
    struct _storage<pack<Ts...>, true, true>
      : _union<
            pack<Ts...>
          , all_of<pack<std::is_trivially_destructible<Ts>...>>::value
        >
    {
        using base_type = _union<
            pack<Ts...>
          , all_of<pack<std::is_trivially_destructible<Ts>...>>::value
        >;

        EGGS_CXX11_CONSTEXPR _storage() EGGS_CXX11_NOEXCEPT
          : base_type{index<0>{}}
          , _which{0}
        {}

        _storage(_storage const& rhs) = default;
        _storage(_storage&& rhs) = default;

        template <std::size_t I, typename ...Args>
        EGGS_CXX11_CONSTEXPR _storage(index<I> which, Args&&... args)
          : base_type{which, std::forward<Args>(args)...}
          , _which{I}
        {}

        _storage& operator=(_storage const& rhs) = default;
        _storage& operator=(_storage&& rhs) = default;

        EGGS_CXX11_CONSTEXPR std::size_t which() const
        {
            return _which;
        }

        using base_type::target;
        using base_type::get;

    protected:
        std::size_t _which;
    };

    template <typename ...Ts>
    using storage = _storage<
        pack<empty, Ts...>
      , all_of<pack<std::is_trivially_copyable<Ts>...>>::value
      , all_of<pack<std::is_trivially_destructible<Ts>...>>::value
    >;
}}}

namespace eggs { namespace variants
{
    template <typename ...Ts>
    class variant;

    namespace detail
    {
        ///////////////////////////////////////////////////////////////////////
        namespace _best_match
        {
            template <typename Ts, std::size_t I = 0>
            struct overloads
            {};

            template <typename T, typename ...Ts, std::size_t I>
            struct overloads<pack<T, Ts...>, I>
              : overloads<pack<Ts...>, I + 1>
            {
                using fun_ptr = index<I>(*)(T);
                operator fun_ptr();
            };

            template <typename F, typename T>
            auto _invoke(F&&, T&&)
             -> decltype(std::declval<F>()(std::declval<T>()));

            struct _fallback {};

            _fallback _invoke(...);

            template <
                typename T, typename U
              , typename R = decltype(_best_match::_invoke(
                    std::declval<T>(), std::declval<U>()))
            >
            struct result_of : R
            {};

            template <typename T, typename U>
            struct result_of<T, U, _fallback>
            {};
        }

        template <typename U, typename ...Ts>
        struct index_of_best_match
          : _best_match::result_of<_best_match::overloads<Ts...>, U>
        {};

    }

    template <typename ...Ts>
    class variant
    {

    public:
        EGGS_CXX11_CONSTEXPR variant() EGGS_CXX11_NOEXCEPT = delete;

        variant(variant const& rhs) = default;

        variant(variant&& rhs) = default;

        template <
            typename U
          , typename Enable = typename std::enable_if<!std::is_same<
                typename std::decay<U>::type, variant
            >::value>::type
          , std::size_t I = detail::index_of_best_match<
                U&&, detail::pack<Ts...>>::value
          , typename T = typename detail::at_index<
                I, detail::pack<Ts...>>::type
        >
        EGGS_CXX11_CONSTEXPR variant(U&& v)
            noexcept(
                std::is_nothrow_constructible<T, U&&>::value)
          : _storage{detail::index<I + 1>{}, std::forward<U>(v)}
        {}

        ~variant() = default;
        variant& operator=(variant const& rhs) = delete;

    private:
        detail::storage<Ts...> _storage;
    };
}}

template <class T, class Base>
struct ref_proxy : Base
{
    using Base::Base;

    ref_proxy()
        : Base()
    {
    }

    ref_proxy(Base ptr)
        : Base(std::move(ptr))
    {
    }
};

template <class T>
struct inplace_ref
{
    explicit inplace_ref(T inner)
        : inner_(inner)
    {
    }

    T inner_;
};

template <class ...Variants>
struct variant_ref
{
    variant_ref() = delete;

    explicit variant_ref(eggs::variants::variant<Variants...> t)
        : inner_storage_(t)
    {
    }

    template <class Source>
    variant_ref(ref_proxy<Source, variant_ref> ptr)
        : inner_storage_(ptr.inner_storage_)
    {}

private:
    eggs::variants::variant<Variants...> inner_storage_;
};

struct option_1
{
    void *a, *b, *c, *d, *e;
};

struct option_2
{
};

using option_ref = variant_ref<option_1, option_2>;


struct qual_option
{
    qual_option(ref_proxy<void, option_ref > type, int quals)
        : type_(type)
        , quals_(quals)
    {
    }

    explicit qual_option(ref_proxy<void, option_ref > type)
        : qual_option(type, 0)
    {
    }

    ref_proxy<void, option_ref > type_;
    int quals_;
};

inline ref_proxy<option_2, option_ref > make_object_1()
{
    return ref_proxy<option_2, option_ref >(option_2());
}

inline ref_proxy<option_2, option_ref > make_object_2()
{
    return make_object_1();
}

inline inplace_ref<qual_option> make_object_3(ref_proxy<option_2, option_ref>&& a0)
{
    return inplace_ref<qual_option>(qual_option(a0));
}

inline ref_proxy<qual_option, inplace_ref<qual_option> > make_object_4(ref_proxy<option_2, option_ref>&& a0)
{
    return make_object_3(std::move(a0));
}


ref_proxy<qual_option, inplace_ref<qual_option> > f() __attribute__((noinline));

ref_proxy<qual_option, inplace_ref<qual_option> > f()
{
    return make_object_4(make_object_2());
}

int main(int argc, char* argv[])
{
    for (;;)
        f();
}

/* { dg-final { scan-tree-dump "Removing load:.*ptr;" "sra" } } */
