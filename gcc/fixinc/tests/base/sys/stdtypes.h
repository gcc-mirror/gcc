

#if defined( SYSZ_STDTYPES_FOR_SUN_CHECK )
#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
#ifndef _GCC_SIZE_T
#define _GCC_SIZE_T
typedef __SIZE_TYPE__ size_t; /* ??? */
#endif
#ifndef __PTRDIFF_TYPE__
#define __PTRDIFF_TYPE__ long int
#endif
#ifndef _GCC_PTRDIFF_T
#define _GCC_PTRDIFF_T
typedef __PTRDIFF_TYPE__ ptrdiff_t; /* result of subtracting two pointers */
#endif
#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif
#ifndef __cplusplus
#ifndef _GCC_WCHAR_T
#define _GCC_WCHAR_T
typedef __WCHAR_TYPE__ wchar_t; /* big enough for biggest char set */
#endif
#endif

#endif  /* SYSZ_STDTYPES_FOR_SUN_CHECK */
