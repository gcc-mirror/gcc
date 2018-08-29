/* { dg-do compile } */
/* { dg-options "-O2" } */
// { dg-additional-options "-Wno-return-type" }
namespace sigc {
    template <class T_type>     struct type_trait     {
    	typedef T_type& pass;
    	typedef const T_type& take;
    	typedef T_type* pointer;
    };
    template <class T_type>     struct type_trait<T_type&>     {
    	typedef T_type& pass;
    };
    template<>     struct type_trait<void>     {
    	typedef void pass;
    };
    template <class T_base, class T_derived>     struct is_base_and_derived     {
    	struct big {
  	    char memory[64];
  	};
    	static big is_base_class_(...);
    	static char is_base_class_(typename type_trait<T_base>::pointer);
    	static const bool value = sizeof(is_base_class_(reinterpret_cast<typename type_trait<T_derived>::pointer>(0))) == sizeof(char);
    };
    struct nil;
    struct functor_base {
    };
    template <class T_functor, bool I_derives_functor_base=is_base_and_derived<functor_base,T_functor>::value>     struct functor_trait     {
    	typedef typename T_functor::result_type result_type;
    	typedef T_functor functor_type;
    };
    struct adaptor_base : public functor_base {
    };
    template <class T_functor, class T_arg1=void,class T_arg2=void,class T_arg3=void,class T_arg4=void,class T_arg5=void,class T_arg6=void,class T_arg7=void, bool I_derives_adaptor_base=is_base_and_derived<adaptor_base,T_functor>::value>     struct deduce_result_type     {
    	typedef typename functor_trait<T_functor>::result_type type;
    };
    template <class T_functor>     struct adaptor_functor
	: public adaptor_base     {
    	template <class T_arg1=void,class T_arg2=void,class T_arg3=void,class T_arg4=void,class T_arg5=void,class T_arg6=void,class T_arg7=void> 	struct deduce_result_type 	{
  	    typedef typename sigc::deduce_result_type<T_functor, T_arg1,T_arg2,T_arg3,T_arg4,T_arg5,T_arg6,T_arg7>::type type;
  	};
    	typedef typename functor_trait<T_functor>::result_type result_type;
    	template <class T_arg1,class T_arg2> 	typename deduce_result_type<T_arg1,T_arg2>::type operator()(T_arg1 _A_arg1,T_arg2 _A_arg2) const 	{
  	    return functor_(_A_arg1,_A_arg2);
  	}
    	explicit adaptor_functor(const T_functor& _A_functor) : functor_(_A_functor) {
	}
    	mutable T_functor functor_;
    };
    template <class T_functor, bool I_isadaptor = is_base_and_derived<adaptor_base, T_functor>::value>     struct adaptor_trait;
    template <class T_functor>     struct adaptor_trait<T_functor, true>     {
    	typedef T_functor adaptor_type;
    };
    template <class T_functor>     struct adaptor_trait<T_functor, false>     {
    	typedef typename functor_trait<T_functor>::functor_type functor_type;
    	typedef adaptor_functor<functor_type> adaptor_type;
    };
    template <class T_functor>     struct adapts
	: public adaptor_base     {
    	typedef typename adaptor_trait<T_functor>::adaptor_type adaptor_type;
    	explicit adapts(const T_functor& _A_functor) : functor_(_A_functor) {
	}
    	mutable adaptor_type functor_;
    };
    template <class T_type>     struct unwrap_reference     {
    	typedef T_type type;
    };
    template <class T_type>     class bound_argument     {
    public:
	bound_argument(const T_type& _A_argument) : visited_(_A_argument) {
	}
    	inline T_type& invoke() {
	}
    	T_type visited_;
    };
    template <int I_location, class T_functor, class T_type1=nil,class T_type2=nil,class T_type3=nil,class T_type4=nil,class T_type5=nil,class T_type6=nil,class T_type7=nil>     struct bind_functor;
    template <class T_functor, class T_bound>     struct bind_functor<0, T_functor, T_bound, nil,nil,nil,nil,nil,nil> : public adapts<T_functor>     {
    	typedef typename adapts<T_functor>::adaptor_type adaptor_type;
    	template <class T_arg1=void,class T_arg2=void,class T_arg3=void,class T_arg4=void,class T_arg5=void,class T_arg6=void,class T_arg7=void> 	struct deduce_result_type      	{
  	    typedef typename adaptor_type::template deduce_result_type<typename type_trait<typename unwrap_reference<T_bound>::type>::pass, typename type_trait<T_arg1>::pass, typename type_trait<T_arg2>::pass, typename type_trait<T_arg3>::pass, typename type_trait<T_arg4>::pass, typename type_trait<T_arg5>::pass, typename type_trait<T_arg6>::pass>::type type;
  	};
    	typedef typename adaptor_type::result_type result_type;
    	result_type operator()() 	{
  	    return this->functor_.template operator()<typename type_trait<typename unwrap_reference<T_bound>::type>::pass> (bound_.invoke());
  	}
    	template <class T_arg1> 	typename deduce_result_type<T_arg1>::type operator()(T_arg1 _A_arg1) 	{
  	    return this->functor_.template operator()<typename type_trait<typename unwrap_reference<T_bound>::type>::pass, typename type_trait<T_arg1>::pass>         (bound_.invoke(), _A_arg1);
  	}
    	bind_functor(typename type_trait<T_functor>::take _A_func, typename type_trait<T_bound>::take _A_bound) : adapts<T_functor>(_A_func), bound_(_A_bound) {
	}
    	bound_argument<T_bound> bound_;
    };
    template <int I_location, class T_bound1, class T_functor> inline bind_functor<I_location, T_functor, T_bound1>     bind(const T_functor& _A_func, T_bound1 _A_b1)     {
    	return bind_functor<I_location, T_functor, T_bound1>(_A_func, _A_b1);
    };
}
struct foo {
    typedef int result_type;
    int operator()(int i, int j);
};
int main() {
    sigc::bind<0>(sigc::bind<0>(foo(),7),8)();
}
