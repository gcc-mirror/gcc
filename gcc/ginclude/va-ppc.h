/* GNU C varargs support for the PowerPC with V.4 calling sequence */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

/* Note that the names in this structure are in the user's namespace, but
   that the V.4 abi explicitly states that these names should be used.  */
typedef struct {
  char gpr;			/* index into the array of 8 GPRs stored in the
				   register save area gpr=0 corresponds to r3,
				   gpr=1 to r4, etc. */
  char fpr;			/* index into the array of 8 FPRs stored in the
				   register save area fpr=0 corresponds to f1,
				   fpr=1 to f2, etc. */
  char *overflow_arg_area;	/* location on stack that holds the next
				   overflow argument */
  char *reg_save_area;		/* where r3:r10 and f1:f8, if saved are stored */
} __gnuc_va_list[1];
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

/* Register save area located below the frame pointer */
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

/* Common code for va_start for both varargs and stdarg.  This depends
   on the format of rs6000_args in rs6000.h.  The fields used are:

   #0	WORDS			# words used for GP regs/stack values
   #1	FREGNO			next available FP register
   #2	NARGS_PROTOTYPE		# args left in the current prototype
   #3	ORIG_NARGS		original value of NARGS_PROTOTYPE
   #4	VARARGS_OFFSET		offset from frame pointer of varargs area */

#define __va_words		__builtin_args_info (0)
#define __va_fregno		__builtin_args_info (1)
#define	__va_nargs		__builtin_args_info (2)
#define __va_orig_nargs		__builtin_args_info (3)
#define __va_varargs_offset	__builtin_args_info (4)

#define __va_start_common(AP, FAKE)					\
__extension__ ({							\
   register int __words = __va_words - FAKE;				\
									\
   (AP)->gpr = (__words < 8) ? __words : 8;				\
   (AP)->fpr = __va_fregno - 33;					\
   (AP)->reg_save_area = (((char *) __builtin_frame_address (0))	\
			  + __va_varargs_offset);			\
   (AP)->overflow_arg_area = ((char *)__builtin_saveregs ()		\
			      + (((__words >= 8) ? __words - 8 : 0)	\
				 * sizeof (long)));			\
   (void)0;								\
})

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
	   && (AP)->gpr + __va_size(TYPE) <= 8)				\
    {									\
      __ptr = __VA_GP_REGSAVE (AP, TYPE);				\
      (AP)->gpr += __va_size (TYPE);					\
    }									\
									\
  else if (!__va_float_p (TYPE) && !__va_aggregate_p (TYPE)		\
	   && (AP)->gpr < 8)						\
    {									\
      (AP)->gpr = 8;							\
      __ptr = (TYPE *) (void *) ((AP)->overflow_arg_area);		\
      (AP)->overflow_arg_area += __va_size (TYPE) * sizeof (long);	\
    }									\
									\
  else if (__va_aggregate_p (TYPE))					\
    {									\
      __ptr = * (TYPE **) (void *) ((AP)->overflow_arg_area);		\
      (AP)->overflow_arg_area += sizeof (TYPE *);			\
    }									\
  else									\
    {									\
      __ptr = (TYPE *) (void *) ((AP)->overflow_arg_area);		\
      (AP)->overflow_arg_area += __va_size (TYPE) * sizeof (long);	\
    }									\
									\
  __ptr;								\
}))

#define va_end(AP)	((void)0)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
