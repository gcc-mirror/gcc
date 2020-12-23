// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename _Tp, _Tp __v>
struct integral_constant
{
};

typedef integral_constant<bool, true> true_type;

void __throw_with_nested_impl (true_type);
