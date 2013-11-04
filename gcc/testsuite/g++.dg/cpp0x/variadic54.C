// { dg-options "-std=gnu++11" }
template<typename F, typename... BoundArgs>
class bound_functor
{
 public:
  bound_functor();
};

template<typename F, typename... BoundArgs>
bound_functor<F, BoundArgs...>::bound_functor()
{
}
