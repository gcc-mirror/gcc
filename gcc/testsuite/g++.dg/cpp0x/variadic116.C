// Origin: PR c++/48320
// { dg-options -std=c++0x }

template<class... T>
struct tuple
{
    typedef int type;
};

template<int... Indices>
struct indices
{
};

template<unsigned i, class Tuple>
struct tuple_element
{
    typedef Tuple type;
};

template<class Tuple,
         int... Indices,
         class Result = tuple<typename tuple_element<Indices, Tuple>::type...> >
Result
f(Tuple&&, indices<Indices...>);


void
foo()
{
    f(tuple<int, char, unsigned> (), indices<2, 1, 0> ());
}
