// { dg-options "-std=gnu++0x" }
template<typename F, typename... BoundArgs>
class bound_functor
{
 public:
  bound_functor(const BoundArgs&... bound_args);
};

template<typename F, typename... BoundArgs>
bound_functor<F, BoundArgs...>::bound_functor(const BoundArgs&...)
{
}
