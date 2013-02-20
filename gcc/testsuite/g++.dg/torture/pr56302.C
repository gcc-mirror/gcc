// PR c++/56302
// { dg-do compile }

typedef __SIZE_TYPE__ size_t;
# define STAP_SDT_ARG_CONSTRAINT        nor
# define _SDT_STRINGIFY(x)              #x
# define _SDT_ARG_CONSTRAINT_STRING(x)  _SDT_STRINGIFY(x)
# define _SDT_ARG(n, x)			\
  [_SDT_S##n] "n" ((_SDT_ARGSIGNED (x) ? 1 : -1) * (int) _SDT_ARGSIZE (x)), \
  [_SDT_A##n] _SDT_ARG_CONSTRAINT_STRING (STAP_SDT_ARG_CONSTRAINT) (_SDT_ARGVAL (x))
#define _SDT_ARGARRAY(x)	(__builtin_classify_type (x) == 14	\
				 || __builtin_classify_type (x) == 5)
# define _SDT_ARGSIGNED(x)	(!_SDT_ARGARRAY (x) \
				 && __sdt_type<__typeof (x)>::__sdt_signed)
# define _SDT_ARGSIZE(x)	(_SDT_ARGARRAY (x) \
				 ? sizeof (void *) : sizeof (x))
# define _SDT_ARGVAL(x)		(x)
template<typename __sdt_T>
struct __sdt_type
{
  static const bool __sdt_signed = false;
};
#define __SDT_ALWAYS_SIGNED(T) \
template<> struct __sdt_type<T> { static const bool __sdt_signed = true; };
__SDT_ALWAYS_SIGNED(signed char)
__SDT_ALWAYS_SIGNED(short)
__SDT_ALWAYS_SIGNED(int)
__SDT_ALWAYS_SIGNED(long)
__SDT_ALWAYS_SIGNED(long long)
template<typename __sdt_E>
struct __sdt_type<__sdt_E[]> : public __sdt_type<__sdt_E *> {};
template<typename __sdt_E, size_t __sdt_N>
struct __sdt_type<__sdt_E[__sdt_N]> : public __sdt_type<__sdt_E *> {};

struct S { char p[8]; };

void
foo (const S &str)
{
  __asm__ __volatile__ ("" : : _SDT_ARG (0, &str));
}
