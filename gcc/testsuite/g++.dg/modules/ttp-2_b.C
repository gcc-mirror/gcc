// { dg-additional-options -fmodules-ts }

module foo;

template <template<typename ...> class Arg4>
struct TPL1
{
};

template <template<typename ...> class Arg4>
struct TPL2
{
};

template <typename> class Arg;


template struct TPL1<Arg>;
