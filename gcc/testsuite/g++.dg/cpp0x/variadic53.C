// { dg-options "-std=gnu++11" }
template<typename F, typename... BoundArgs>
class bound_functor
{
 public:
  typedef typename F::result_type result_type;

  template<typename... Args>
  typename F::result_type operator()(Args&... args);
};

template<typename F, typename... BoundArgs>
template<typename... Args>
typename F::result_type 
bound_functor<F, BoundArgs...>::operator()(Args&... args)
{
}
