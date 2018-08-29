// PR c++/72415
// { dg-options "-std=c++17 -fconcepts" }

template<int... Indices>
struct indices {};

template<typename Dummy>
struct foo_type {
    template<int... Indices>
    static void impl(indices<Indices...>)
        requires (... && (Indices, true));

    static auto caller()
    { return impl(indices<0, 1, 2> {}); }
};

int main()
{
    // internal compiler error: in satisfy_predicate_constraint, at cp/constraint.cc:2013
    foo_type<void>::caller();
}
