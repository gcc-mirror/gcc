/* Format checking tests: common header.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

#include <stdarg.h>
#include <stddef.h>

#ifndef _WINT_T
#ifndef __WINT_TYPE__
#define __WINT_TYPE__ unsigned int
#endif
typedef __WINT_TYPE__ wint_t;
#endif

/* Kludges to get types corresponding to size_t and ptrdiff_t.  */
#define unsigned signed
typedef __SIZE_TYPE__ signed_size_t;
/* We also use this type to approximate ssize_t.  */
typedef __SIZE_TYPE__ ssize_t;
#undef unsigned
#define signed /* Type might or might not have explicit 'signed'.  */
typedef unsigned __PTRDIFF_TYPE__ unsigned_ptrdiff_t;
#undef signed

__extension__ typedef long long int llong;
__extension__ typedef unsigned long long int ullong;

/* %q formats want a "quad"; GCC considers this to be a long long.  */
typedef llong quad_t;
typedef ullong u_quad_t;

/* This next definition is a kludge.  When GCC has a <stdint.h> it
   should be used.
*/
/* (T *) if E is zero, (void *) otherwise.  */
#define type_if_not(T, E) __typeof__(0 ? (T *)0 : (void *)(E))

/* (T *) if E is nonzero, (void *) otherwise.  */
#define type_if(T, E) type_if_not(T, !(E))

/* Combine pointer types, all but one (void *).  */
#define type_comb2(T1, T2) __typeof__(0 ? (T1)0 : (T2)0)
#define type_comb3(T1, T2, T3) type_comb2(T1, type_comb2(T2, T3))

#define maybe_int_ptr type_if(int, sizeof(int) == sizeof(llong))
#define maybe_uint_ptr type_if(unsigned int, sizeof(unsigned int) == sizeof(ullong))
#define maybe_long_ptr type_if(long, sizeof(long) == sizeof(llong) && sizeof(long) > sizeof(int))
#define maybe_ulong_ptr type_if(unsigned long, sizeof(unsigned long) == sizeof(ullong) && sizeof(unsigned long) > sizeof(unsigned int))
#define maybe_long_long_ptr type_if(llong, sizeof(llong) > sizeof(long))
#define maybe_ulong_long_ptr type_if(ullong, sizeof(ullong) > sizeof(unsigned long))

#define intmax_type_ptr type_comb3(maybe_int_ptr, maybe_long_ptr, maybe_long_long_ptr)
#define uintmax_type_ptr type_comb3(maybe_uint_ptr, maybe_ulong_ptr, maybe_ulong_long_ptr)

typedef __typeof__(*((intmax_type_ptr)0)) intmax_t;
typedef __typeof__(*((uintmax_type_ptr)0)) uintmax_t;

#if __STDC_VERSION__ < 199901L
#define restrict /* "restrict" not in old C standard.  */
#endif

/* This may not be correct in the particular case, but allows the
   prototypes to be declared, and we don't try to link.
*/
typedef struct _FILE FILE;
extern FILE *stdin;
extern FILE *stdout;

extern int fprintf (FILE *restrict, const char *restrict, ...);
extern int printf (const char *restrict, ...);
extern int fprintf_unlocked (FILE *restrict, const char *restrict, ...);
extern int printf_unlocked (const char *restrict, ...);
extern int sprintf (char *restrict, const char *restrict, ...);
extern int vfprintf (FILE *restrict, const char *restrict, va_list);
extern int vprintf (const char *restrict, va_list);
extern int vsprintf (char *restrict, const char *restrict, va_list);
extern int snprintf (char *restrict, size_t, const char *restrict, ...);
extern int vsnprintf (char *restrict, size_t, const char *restrict, va_list);

extern int fscanf (FILE *restrict, const char *restrict, ...);
extern int scanf (const char *restrict, ...);
extern int sscanf (const char *restrict, const char *restrict, ...);
extern int vfscanf (FILE *restrict, const char *restrict, va_list);
extern int vscanf (const char *restrict, va_list);
extern int vsscanf (const char *restrict, const char *restrict, va_list);

extern char *gettext (const char *);
extern char *dgettext (const char *, const char *);
extern char *dcgettext (const char *, const char *, int);

struct tm;

extern size_t strftime (char *restrict, size_t, const char *restrict,
			const struct tm *restrict);

extern ssize_t strfmon (char *restrict, size_t, const char *restrict, ...);
