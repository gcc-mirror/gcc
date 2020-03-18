template<typename _Tp>
struct remove_reference
{ typedef _Tp FOO; };

template<typename _Tp>
void forward (typename remove_reference<_Tp>::FOO const& __t)
{
}

template<typename _Callable>
void __invoke(_Callable const & __fn)
{
  forward<_Callable const>(__fn);
}

class _State_baseV2
{
public:
  void _M_set_result()
  {
    __invoke (&_State_baseV2::_M_do_set);
  }

  void _M_do_set();
};

