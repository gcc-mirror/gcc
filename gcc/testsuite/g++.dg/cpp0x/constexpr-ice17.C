// PR c++/67687
// { dg-do compile { target c++11 } }

template <unsigned int Coordinate>
struct dimension{
    template<typename ... T>
    constexpr dimension(T...){}
};

struct accessor_base{
    template<typename ... T>
    constexpr accessor_base(T...){}
};

template <typename ArgType, typename Pair>
struct accessor_mixed{

private:
    static constexpr accessor_base s_args_constexpr{dimension<Pair::first>{0} };
};
