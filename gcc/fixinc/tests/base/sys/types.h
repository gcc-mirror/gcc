

#if defined( GNU_TYPES_CHECK )
#ifndef __PTRDIFF_TYPE__
#define __PTRDIFF_TYPE__ long int
#endif
#if !defined(_GCC_PTRDIFF_T)
#define _GCC_PTRDIFF_T
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#endif
 /* long int */
#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
#if !defined(_GCC_SIZE_T)
#define _GCC_SIZE_T
typedef __SIZE_TYPE__ size_t;
#endif
 /* uint_t */
#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif
#if !defined(_GCC_WCHAR_T) && !defined(__cplusplus)
#define _GCC_WCHAR_T
typedef __WCHAR_TYPE__ wchar_t;
#endif
 /* ushort_t */
#endif  /* GNU_TYPES_CHECK */
