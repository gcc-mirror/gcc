/* GNU C varargs support for the PowerPC with either the V.4 or Windows NT calling sequences */

#ifndef _WIN32
/* System V.4 support */
/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

#ifndef _SYS_VA_LIST_H
#define _SYS_VA_LIST_H		/* Solaris sys/va_list.h */

/* Solaris decided to rename overflow_arg_area to input_arg_area,
   so handle it via a macro.  */
#define __va_overflow(AP) (AP)->overflow_arg_area

/* Note that the names in this structure are in the user's namespace, but
   that the V.4 abi explicitly states that these names should be used.  */
typedef struct __va_list_tag {
  char gpr;			/* index into the array of 8 GPRs stored in the
				   register save area gpr=0 corresponds to r3,
				   gpr=1 to r4, etc. */
  char fpr;			/* index into the array of 8 FPRs stored in the
				   register save area fpr=0 corresponds to f1,
				   fpr=1 to f2, etc. */
  char *overflow_arg_area;	/* location on stack that holds the next
				   overflow argument */
  char *reg_save_area;		/* where r3:r10 and f1:f8, if saved are stored */
} __va_list[1], __gnuc_va_list[1];

#else /* _SYS_VA_LIST */

typedef __va_list __gnuc_va_list;
#define __va_overflow(AP) (AP)->input_arg_area

#endif /* not _SYS_VA_LIST */
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

/* Register save area located below the frame pointer */
#ifndef __VA_PPC_H__
#define __VA_PPC_H__
typedef struct {
  long   __gp_save[8];		/* save area for GP registers */
  double __fp_save[8];		/* save area for FP registers */
} __va_regsave_t;

/* Macros to access the register save area */
/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */
#define __VA_FP_REGSAVE(AP,TYPE)					\
  ((TYPE *) (void *) (&(((__va_regsave_t *)				\
			 (AP)->reg_save_area)->__fp_save[(int)(AP)->fpr])))

#define __VA_GP_REGSAVE(AP,TYPE)					\
  ((TYPE *) (void *) (&(((__va_regsave_t *)				\
			 (AP)->reg_save_area)->__gp_save[(int)(AP)->gpr])))

/* Common code for va_start for both varargs and stdarg.  We allow all
   the work to be done by __builtin_saveregs.  It returns a pointer to
   a va_list that was constructed on the stack; we must simply copy it
   to the user's variable.  */

#define __va_start_common(AP, FAKE) \
  __builtin_memcpy ((AP), __builtin_saveregs (), sizeof(__gnuc_va_list))

#ifdef _STDARG_H /* stdarg.h support */

/* Calling __builtin_next_arg gives the proper error message if LASTARG is
   not indeed the last argument.  */
#define va_start(AP,LASTARG) \
  (__builtin_next_arg (LASTARG), __va_start_common (AP, 0))

#else /* varargs.h support */

#define va_start(AP) __va_start_common (AP, 1)
#define va_alist __va_1st_arg
#define va_dcl register int va_alist; ...

#endif /* _STDARG_H */

#ifdef _SOFT_FLOAT
#define __va_float_p(TYPE)	0
#else
#define __va_float_p(TYPE)	(__builtin_classify_type(*(TYPE *)0) == 8)
#endif

#define __va_longlong_p(TYPE) \
  ((__builtin_classify_type(*(TYPE *)0) == 1) && (sizeof(TYPE) == 8))

#define __va_aggregate_p(TYPE)	(__builtin_classify_type(*(TYPE *)0) >= 12)
#define __va_size(TYPE)		((sizeof(TYPE) + sizeof (long) - 1) / sizeof (long))

#define va_arg(AP,TYPE)							\
__extension__ (*({							\
  register TYPE *__ptr;							\
									\
  if (__va_float_p (TYPE) && (AP)->fpr < 8)				\
    {									\
      __ptr = __VA_FP_REGSAVE (AP, TYPE);				\
      (AP)->fpr++;							\
    }									\
									\
  else if (__va_aggregate_p (TYPE) && (AP)->gpr < 8)			\
    {									\
      __ptr = * __VA_GP_REGSAVE (AP, TYPE *);				\
      (AP)->gpr++;							\
    }									\
									\
  else if (!__va_float_p (TYPE) && !__va_aggregate_p (TYPE)		\
	   && (AP)->gpr + __va_size(TYPE) <= 8				\
	   && (!__va_longlong_p(TYPE)					\
	       || (AP)->gpr + __va_size(TYPE) <= 8))			\
    {									\
      if (__va_longlong_p(TYPE) && ((AP)->gpr & 1) != 0)		\
	(AP)->gpr++;							\
									\
      __ptr = __VA_GP_REGSAVE (AP, TYPE);				\
      (AP)->gpr += __va_size (TYPE);					\
    }									\
									\
  else if (!__va_float_p (TYPE) && !__va_aggregate_p (TYPE)		\
	   && (AP)->gpr < 8)						\
    {									\
      (AP)->gpr = 8;							\
      __ptr = (TYPE *) (void *) (__va_overflow(AP));			\
      __va_overflow(AP) += __va_size (TYPE) * sizeof (long);		\
    }									\
									\
  else if (__va_aggregate_p (TYPE))					\
    {									\
      __ptr = * (TYPE **) (void *) (__va_overflow(AP));			\
      __va_overflow(AP) += sizeof (TYPE *);				\
    }									\
  else									\
    {									\
      __ptr = (TYPE *) (void *) (__va_overflow(AP));			\
      __va_overflow(AP) += __va_size (TYPE) * sizeof (long);		\
    }									\
									\
  __ptr;								\
}))

#define va_end(AP)	((void)0)

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) *(dest) = *(src)

#endif /* __VA_PPC_H__ */
#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */


#else
/* Windows NT */
/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef char *__gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#define __va_start_common(AP, LASTARG, FAKE)				\
  ((__builtin_saveregs ()), ((AP) = ((char *) &LASTARG) + __va_rounded_size (AP)), 0)

#ifdef _STDARG_H /* stdarg.h support */

/* Calling __builtin_next_arg gives the proper error message if LASTARG is
   not indeed the last argument.  */
#define va_start(AP,LASTARG)						\
  (__builtin_saveregs (),						\
   (AP) = __builtin_next_arg (LASTARG),					\
   0)

#else /* varargs.h support */

#define va_start(AP)							\
  (__builtin_saveregs (),						\
   (AP) = __builtin_next_arg (__va_1st_arg) - sizeof (int),		\
   0)

#define va_alist __va_1st_arg
#define va_dcl register int __va_1st_arg; ...

#endif /* _STDARG_H */

#define __va_rounded_size(TYPE) ((sizeof (TYPE) + 3) & ~3)
#define __va_align(AP, TYPE)						\
     ((((unsigned long)(AP)) + ((sizeof (TYPE) >= 8) ? 7 : 3))		\
      & ~((sizeof (TYPE) >= 8) ? 7 : 3))

#define va_arg(AP,TYPE)							\
( *(TYPE *)((AP = (char *) (__va_align(AP, TYPE)			\
			    + __va_rounded_size(TYPE)))			\
	    - __va_rounded_size(TYPE)))

#define va_end(AP)	((void)0)

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
#endif /* Windows NT */
