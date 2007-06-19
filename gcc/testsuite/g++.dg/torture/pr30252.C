/* { dg-do run } */
/* { dg-options "-fstrict-aliasing" } */

extern "C" void abort (void);
namespace sigc {
  template <class T_type>
  struct type_trait
  {
    typedef T_type& pass;
    typedef const T_type& take;
    typedef T_type* pointer;
  };
  template <class T_base, class T_derived>
  struct is_base_and_derived
  {
    struct big {
      char memory[64];
    };
    static big is_base_class_(...);
    static char is_base_class_(typename type_trait<T_base>::pointer);
    static const bool value =
    sizeof(is_base_class_(reinterpret_cast<typename type_trait<T_derived>::pointer>(0))) ==
    sizeof(char);
  };
  struct nil;
  struct functor_base {};
  template <class T_functor, bool I_derives_functor_base=is_base_and_derived<functor_base,T_functor>::value>
  struct functor_trait
  {
  };
  template <class T_functor>
  struct functor_trait<T_functor,true>
  {
    typedef typename T_functor::result_type result_type;
    typedef T_functor functor_type;
  };
  template <class T_arg1, class T_return>
  class pointer_functor1 : public functor_base
  {
    typedef T_return (*function_type)(T_arg1);
    function_type func_ptr_;
  public:
    typedef T_return result_type;
    explicit pointer_functor1(function_type _A_func): func_ptr_(_A_func) {}
    T_return operator()(typename type_trait<T_arg1>::take _A_a1) const
    { return func_ptr_(_A_a1); }
  };
  template <class T_arg1, class T_return>
  inline pointer_functor1<T_arg1, T_return>
  ptr_fun1(T_return (*_A_func)(T_arg1))
  { return pointer_functor1<T_arg1, T_return>(_A_func); }
  struct adaptor_base : public functor_base {};
  template <class T_functor,
    class T_arg1=void,
    bool I_derives_adaptor_base=is_base_and_derived<adaptor_base,T_functor>::value>
  struct deduce_result_type
  { typedef typename functor_trait<T_functor>::result_type type; };
  template <class T_functor>
  struct adaptor_functor : public adaptor_base
  {
    template <class T_arg1=void>
    struct deduce_result_type
    { typedef typename sigc::deduce_result_type<T_functor, T_arg1>::type type; };
    typedef typename functor_trait<T_functor>::result_type result_type;
    result_type
    operator()() const;
    template <class T_arg1>
    typename deduce_result_type<T_arg1>::type
    operator()(T_arg1 _A_arg1) const
    { return functor_(_A_arg1); }
    explicit adaptor_functor(const T_functor& _A_functor)
      : functor_(_A_functor)
    {}
    mutable T_functor functor_;
  };
  template <class T_functor>
  typename adaptor_functor<T_functor>::result_type
  adaptor_functor<T_functor>::operator()() const
  { return functor_(); }
  template <class T_functor, bool I_isadaptor = is_base_and_derived<adaptor_base, T_functor>::value> struct adaptor_trait;
  template <class T_functor>
  struct adaptor_trait<T_functor, true>
  {
    typedef T_functor adaptor_type;
  };
  template <class T_functor>
  struct adaptor_trait<T_functor, false>
  {
    typedef typename functor_trait<T_functor>::functor_type functor_type;
    typedef adaptor_functor<functor_type> adaptor_type;
  };
  template <class T_functor>
  struct adapts : public adaptor_base
  {
    typedef typename adaptor_trait<T_functor>::adaptor_type adaptor_type;
    explicit adapts(const T_functor& _A_functor)
      : functor_(_A_functor)
    {}
    mutable adaptor_type functor_;
  };
  template <class T_type>
  struct reference_wrapper
  {
  };
  template <class T_type>
  struct unwrap_reference
  {
    typedef T_type type;
  };
  template <class T_type>
  class bound_argument
  {
  public:
    bound_argument(const T_type& _A_argument)
      : visited_(_A_argument)
    {}
    inline T_type& invoke()
    { return visited_; }
    T_type visited_;
  };
  template <class T_wrapped>
  class bound_argument< reference_wrapper<T_wrapped> >
  {
  };
  template <int I_location, class T_functor, class T_type1=nil>
  struct bind_functor;
  template <class T_functor, class T_type1>
  struct bind_functor<-1, T_functor, T_type1> : public adapts<T_functor>
  {
    typedef typename adapts<T_functor>::adaptor_type adaptor_type;
    typedef typename adaptor_type::result_type result_type;
    result_type
    operator()()
    {
      return this->functor_.template operator()<typename type_trait<typename unwrap_reference<T_type1>::type>::pass> (bound1_.invoke());
    }
    bind_functor(typename type_trait<T_functor>::take _A_func, typename type_trait<T_type1>::take _A_bound1)
      : adapts<T_functor>(_A_func), bound1_(_A_bound1)
    {}
    bound_argument<T_type1> bound1_;
  };
  template <class T_type1, class T_functor>
  inline bind_functor<-1, T_functor,
		      T_type1>
  bind(const T_functor& _A_func, T_type1 _A_b1)
  { return bind_functor<-1, T_functor,
      T_type1>
      (_A_func, _A_b1);
  }
  namespace internal {
    struct slot_rep;
    typedef void* (*hook)(slot_rep *);
    struct slot_rep
    {
      hook call_;
    };
  }
  class slot_base : public functor_base
  {
  public:
    typedef internal::slot_rep rep_type;
    explicit slot_base(rep_type* rep)
      : rep_(rep)
    {
    }
    mutable rep_type *rep_;
  };
  namespace internal {
    template <class T_functor>
    struct typed_slot_rep : public slot_rep
    {
      typedef typename adaptor_trait<T_functor>::adaptor_type adaptor_type;
      adaptor_type functor_;
      inline typed_slot_rep(const T_functor& functor)
	: functor_(functor)
      {
      }
    };
    template<class T_functor>
    struct slot_call0
    {
      static void *call_it(slot_rep* rep)
      {
	typedef typed_slot_rep<T_functor> typed_slot;
	typed_slot *typed_rep = static_cast<typed_slot*>(rep);
	return (typed_rep->functor_)();
      }
      static hook address()
      {
	return &call_it;
      }
    };
  }

  class slot0 : public slot_base
  {
  public:
    typedef void * (*call_type)(rep_type*);
    inline void *operator()() const
    {
      return slot_base::rep_->call_ (slot_base::rep_);
    }
    template <class T_functor>
    slot0(const T_functor& _A_func)
      : slot_base(new internal::typed_slot_rep<T_functor>(_A_func))
    {
      slot_base::rep_->call_ = internal::slot_call0<T_functor>::address();
    }
  };
}
struct A
{
  static void *foo (void *p) { return p; }
  typedef sigc::slot0 C;
  C bar();
};
A::C A::bar ()
{
  return sigc::bind (sigc::ptr_fun1 (&A::foo), (void*)0);
}
int main (void)
{
  A a;
  if (a.bar ()() != 0)
    abort ();
}
