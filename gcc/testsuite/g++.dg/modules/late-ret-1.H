// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _Iterator>
struct reverse_iterator
{
  _Iterator base() const;
};

template<typename _Iterator>
reverse_iterator<_Iterator> __make_reverse_iterator (_Iterator __i);

template<typename _Iterator>
auto __niter_base(reverse_iterator<_Iterator> __it)
  -> decltype (__make_reverse_iterator(__niter_base(__it.base())))
{ return __make_reverse_iterator(__niter_base(__it.base())); }
