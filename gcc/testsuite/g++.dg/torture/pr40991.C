/* { dg-options "-std=gnu++0x" } */
/* { dg-additional-options "-Wno-return-type" } */

typedef __SIZE_TYPE__ size_t;
namespace std __attribute__ ((__visibility__ ("default"))) {
    using ::size_t;
    void __throw_bad_function_call() __attribute__((__noreturn__));
}
inline void* operator new(std::size_t, void* __p) throw() {
    return __p;
}
namespace std {
    class type_info {
    public:
	bool operator==(const type_info& __arg) const { }
    };
    namespace tr1 {
	template<typename _Tp, _Tp __v> struct integral_constant { };
	typedef integral_constant<bool, true> true_type;
	template<typename _Res, typename... _ArgTypes>
	struct _Maybe_unary_or_binary_function { };
	class _Undefined_class;
	union _Nocopy_types {
	    void (_Undefined_class::*_M_member_pointer)();
	};
	union _Any_data {
	    void* _M_access() {
		return &_M_pod_data[0];
	    }
	    char _M_pod_data[sizeof(_Nocopy_types)];
	};
	enum _Manager_operation { __get_type_info, __get_functor_ptr, __clone_functor, __destroy_functor };
	template<typename _Tp> struct _Simple_type_wrapper {
	    _Simple_type_wrapper(_Tp __value) : __value(__value) { }
	    _Tp __value;
	};
	template<typename _Signature> class function;
	class _Function_base {
	public:
	    static const std::size_t _M_max_size = sizeof(_Nocopy_types);
	    static const std::size_t _M_max_align = __alignof__(_Nocopy_types);
	    template<typename _Functor> class _Base_manager {
		static const bool __stored_locally = (sizeof(_Functor) <= _M_max_size && __alignof__(_Functor) <= _M_max_align && (_M_max_align % __alignof__(_Functor) == 0));
		typedef integral_constant<bool, __stored_locally> _Local_storage;
	    public:
		static bool _M_manager(_Any_data& __dest, const _Any_data& __source, _Manager_operation __op) { }
		static void _M_init_functor(_Any_data& __functor, const _Functor& __f) {
		    _M_init_functor(__functor, __f, _Local_storage());
		}
		template<typename _Class, typename _Tp> static bool _M_not_empty_function(_Tp _Class::* const& __mp) {
		    return __mp;
		}
		static void _M_init_functor(_Any_data& __functor, const _Functor& __f, true_type) {
		    new (__functor._M_access()) _Functor(__f);
		}
	    };
	    ~_Function_base() {
		if (_M_manager) _M_manager(_M_functor, _M_functor, __destroy_functor);
	    }
	    bool _M_empty() const {
		return !_M_manager;
	    }
	    typedef bool (*_Manager_type)(_Any_data&, const _Any_data&, _Manager_operation);
	    _Any_data _M_functor;
	    _Manager_type _M_manager;
	};
	template<typename _Signature, typename _Functor> class _Function_handler;
	template<typename _Class, typename _Member, typename _Res, typename... _ArgTypes> class _Function_handler<_Res(_ArgTypes...), _Member _Class::*> : public _Function_handler<void(_ArgTypes...), _Member _Class::*> {
	public:
	    static _Res _M_invoke(const _Any_data& __functor, _ArgTypes... __args) { }
	};
	template<typename _Class, typename _Member, typename... _ArgTypes> class _Function_handler<void(_ArgTypes...), _Member _Class::*> : public _Function_base::_Base_manager< _Simple_type_wrapper< _Member _Class::* > > { };
	template<typename _Res, typename... _ArgTypes> class function<_Res(_ArgTypes...)> : public _Maybe_unary_or_binary_function<_Res, _ArgTypes...>, private _Function_base {
	    typedef _Res _Signature_type(_ArgTypes...);
	    struct _Useless { };
	public:
	    template<typename _Functor> function(_Functor __f, _Useless = _Useless());
	    _Res operator()(_ArgTypes... __args) const;
	    const type_info& target_type() const;
	    typedef _Res (*_Invoker_type)(const _Any_data&, _ArgTypes...);
	    _Invoker_type _M_invoker;
	};
	template<typename _Res, typename... _ArgTypes> template<typename _Functor> function<_Res(_ArgTypes...)>:: function(_Functor __f, _Useless) : _Function_base() {
	    typedef _Function_handler<_Signature_type, _Functor> _My_handler;
	    if (_My_handler::_M_not_empty_function(__f)) {
		_M_invoker = &_My_handler::_M_invoke;
		_M_manager = &_My_handler::_M_manager;
		_My_handler::_M_init_functor(_M_functor, __f);
	    }
	}
	template<typename _Res, typename... _ArgTypes> _Res function<_Res(_ArgTypes...)>:: operator()(_ArgTypes... __args) const {
	    if (_M_empty()) {
		__throw_bad_function_call();
	    }
	    return _M_invoker(_M_functor, __args...);
	}
	template<typename _Res, typename... _ArgTypes> const type_info& function<_Res(_ArgTypes...)>:: target_type() const {
	    if (_M_manager) {
		_Any_data __typeinfo_result;
		_M_manager(__typeinfo_result, _M_functor, __get_type_info);
	    }
	}
    }
}
struct X {
    int bar;
};
void test05() {
    using std::tr1::function;
    X x;
    function<int(X&)> frm(&X::bar);
    frm(x) == 17;
    typeid(int X::*) == frm.target_type();
}
