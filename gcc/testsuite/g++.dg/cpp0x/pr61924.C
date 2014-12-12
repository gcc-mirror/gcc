// PR c++/61924
// { dg-do compile { target c++11 } }

struct function
{
  template < typename _Functor > function (_Functor); // { dg-error "never defined" }
};

template < typename > struct RetryingRpc
{
  template < typename StubType> RetryingRpc (StubType, function =[]{});
};

void fn()
{
  RetryingRpc<int> rpc(0, []{});
}
