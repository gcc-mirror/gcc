#ifndef _STDDEF_H
#ifndef _STDDEF_H_
#define _STDDEF_H
#define _STDDEF_H_

#ifndef __sys_stdtypes_h
/* This avoids lossage on Sunos but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */

/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE__TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */

/* Signed type of difference of two pointers.  */

#ifndef _PTRDIFF_T	/* in case <sys/types.h> has defined it. */
#ifndef _T_PTRDIFF
#ifndef __PTRDIFF_T
#ifndef _PTRDIFF_T_
#ifndef ___int_ptrdiff_t_h
#define _PTRDIFF_T
#define _T_PTRDIFF
#define __PTRDIFF_T
#define _PTRDIFF_T_
#define ___int_ptrdiff_t_h
#ifndef __PTRDIFF_TYPE__
#define __PTRDIFF_TYPE__ long int
#endif
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#endif /* ___int_ptrdiff_t_h */
#endif /* _PTRDIFF_T_ */
#endif /* __PTRDIFF_T */
#endif /* _T_PTRDIFF */
#endif /* _PTRDIFF_T */

/* Unsigned type of `sizeof' something.  */

#ifndef _SIZE_T	/* in case <sys/types.h> has defined it. */
#ifndef _T_SIZE
#ifndef __SIZE_T
#ifndef _SIZE_T_
#ifndef ___int_size_t_h
#define _SIZE_T
#define _T_SIZE
#define __SIZE_T
#define _SIZE_T_
#define ___int_size_t_h
#ifndef __SIZE_TYPE__
#define __SIZE_TYPE__ long unsigned int
#endif
typedef __SIZE_TYPE__ size_t;
#endif /* ___int_size_t_h */
#endif /* _SIZE_T_ */
#endif /* __SIZE_T */
#endif /* _T_SIZE */
#endif /* _SIZE_T */

/* Data type for wide chars.  */

#ifndef _WCHAR_T
#ifndef _T_WCHAR_
#ifndef _T_WCHAR
#ifndef __WCHAR_T
#ifndef _WCHAR_T_
#ifndef ___int_wchar_t_h
#define _WCHAR_T
#define _T_WCHAR_
#define _T_WCHAR
#define __WCHAR_T
#define _WCHAR_T_
#define ___int_wchar_t_h
#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif
typedef __WCHAR_TYPE__ wchar_t;
#endif
#endif
#endif
#endif
#endif
#endif

#endif /* __sys_stdtypes_h */

/* A null pointer constant.  */

#undef NULL		/* in case <stdio.h> has defined it. */
#define NULL ((void *)0)

/* Offset of member MEMBER in a struct of type TYPE.  */

#define offsetof(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)

#endif /* _STDDEF_H_ */
#endif /* _STDDEF_H */
