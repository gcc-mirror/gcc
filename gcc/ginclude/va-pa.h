
/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef double *__gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)
#if __GNUC__ > 1
#define __va_ellipsis ...
#define __gnuc_va_start(AP) ((AP) = (va_list)__builtin_saveregs())
#else
#define va_alist __va_a__, __va_b__, __va_c__, __va_d__
#define __va_ellipsis 
#define __gnuc_va_start(AP)\
  (AP) = (double *) &__va_a__, &__va_b__, &__va_c__, &__va_d__, \
  (AP) = (double *)((char *)(AP) + 4)
#endif /* __GNUC__ > 1 */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that LASTARG is correct.  */
#ifdef _STDARG_H
#define va_start(AP,LASTARG) \
  (__builtin_next_arg (LASTARG), __gnuc_va_start (AP))
#else
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl long va_alist; __va_ellipsis
#define va_start(AP) __gnuc_va_start (AP)
#endif

#define va_arg(AP,TYPE)						\
  (*(sizeof(TYPE) > 8 ?						\
   ((AP = (__gnuc_va_list) ((char *)AP - sizeof (int))),	\
    (((TYPE *) (void *) (*((int *) (AP))))))			\
   :((AP =							\
      (__gnuc_va_list) ((long)((char *)AP - sizeof (TYPE))	\
			& (sizeof(TYPE) > 4 ? ~0x7 : ~0x3))),	\
     (((TYPE *) (void *) ((char *)AP + ((8 - sizeof(TYPE)) % 4)))))))

#ifndef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#endif
#define va_end(AP)	((void)0)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
