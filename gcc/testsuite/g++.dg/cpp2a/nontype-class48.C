// PR c++/99683
// { dg-do compile { target c++20 } }

template<auto V>
struct nttp_tag {};

template<typename T>
struct type_tag {};


/************************************************/
template<bool is_type>
struct use_ctad
{
    template<auto V> requires (!is_type)
    constexpr use_ctad(nttp_tag<V>) {}
};

template<auto V>
use_ctad(nttp_tag<V>) -> use_ctad<false>;

/**********************************************/
template<use_ctad t>
struct wrapper 
{
    template<typename Tag>
    wrapper(Tag);
};

template<typename Tag>
wrapper(Tag) -> wrapper<use_ctad{Tag()}>;

int main()
{
    wrapper t{nttp_tag<42>{}};
}
