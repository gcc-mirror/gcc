#if __GNUC__ > 1

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct
{
  char *__va_stack_start;	/* Real start of stack args. */
  char *__va_int;		/* Pointer to the general register */
				/* args and stack. */
  char *__va_float;		/* Pointer to the fp register args.  */
  char *__va_double;
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H
#define va_start(AP,LASTARG) ((AP) = *(__gnuc_va_list *)__builtin_saveregs())
#else
#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    int __builtin_va_alist; ...
#define va_start(AP) ((AP) = *(__gnuc_va_list *)__builtin_saveregs())
#endif /* _STDARG_H */

/* Handle pass by invisible reference and voids left by aligned */
/* doubles. */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define va_arg(AP, TYPE) \
  (((__va_rounded_size(TYPE) <= 8)				       	\
    ? ((AP).__va_int -= __va_rounded_size (TYPE),			\
       (AP).__va_float -= __va_rounded_size (TYPE),			\
       (AP).__va_double -= __va_rounded_size (TYPE),			\
       (__alignof__ (TYPE) > 4 ? ((int)(AP).__va_int &= ~(0x7),		\
				  (int)(AP).__va_float &= ~(0x7),	\
				  (int)(AP).__va_double &= ~(0x7)) : 0))\
    : (int)((AP).__va_int -= sizeof (TYPE *),				\
	    (AP).__va_float -= sizeof (TYPE *),				\
	    (AP).__va_double -= sizeof (TYPE *))),			\
   (((AP).__va_int < (AP).__va_stack_start				\
     || __builtin_classify_type (* (TYPE *) 0) != 8)			\
    ? ((__va_rounded_size(TYPE) <= 8) ? *(TYPE *)(AP).__va_int		\
       : **(TYPE **)(AP).__va_int)					\
    : ((__va_rounded_size(TYPE) <= 4) ? *(TYPE *)(AP).__va_float	\
       : ((__va_rounded_size(TYPE) <= 8) ? *(TYPE *)(AP).__va_double	\
	  : **(TYPE **)(AP).__va_int))))


void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#define va_end(AP)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */

#else /* not __GNUCC_ > 1 */

typedef char *va_list;

/* __builtin_saveregs () tickles a bug in the pa-risc gcc 1.39 port, */
/* so don't use it for varargs. Obviously the stdarg stuff doesn't */
/* work very well. */ 

#ifdef _STDARG_H
#define va_start(AP,LASTARG) \
  (__builtin_saveregs(), (AP) = __builtin_next_arg ())

/* gcc1 doesn't implement pass by invisible reference */ 
#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define va_arg(AP,TYPE) \
  ((AP) -= __va_rounded_size (TYPE), (__alignof__ (TYPE) > 4 ?	\
				      (int)AP &= ~(0x7) : 0),	\
   *(TYPE *)(AP))

void va_end (va_list);		/* Defined in libgcc.a */
#define va_end(AP)
#else /* _STDARG_H */
#define va_alist __va_a__, __va_b__, __va_c__, __va_d__
#define va_dcl int __va_a__, __va_b__, __va_c__, __va_d__;
#define va_start(list) list = (char *) &__va_a__, &__va_b__, &__va_c__, \
  &__va_d__

# define va_arg(list,mode) *(mode *) ((int) (list = (char *) \
	(((int) list + sizeof(int /*__va_a__*/) - sizeof(mode)) & ~(sizeof(mode)-1)) \
	- sizeof(int /*__va_a__*/)) + sizeof(int /*__va_a__*/))
  
#define va_end(list)

#endif /* _STDARG_H */
#endif /* not __GNUCC__ > 1 */
