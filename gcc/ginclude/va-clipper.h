/* GNU C varargs and stdargs support for Clipper.  */

/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct
{
  double __va_f[2];			/* saved floating regs f0,f1 */
  int __va_r[2];			/* saved general regs r0,r1 */
  int __va_ap;				/* pointer to stack args */
  int __va_num;				/* number of args processed */
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */


#if defined (_STDARG_H) || defined (_VARARGS_H)
#define va_list __gnuc_va_list
#define _VA_LIST
#define _VA_LIST_

#ifdef _STDARG_H
#define va_start(AP,LASTARG)			\
  ((AP) = *(va_list *)__builtin_saveregs(),	\
   (AP).__va_num = __builtin_args_info (0),	\
   (AP).__va_ap += __builtin_args_info (1))
#else
#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    va_list __builtin_va_alist; ...
#define va_start(AP)				\
  ((AP) = *(va_list *)__builtin_saveregs(),	\
   (AP).__va_num = __builtin_args_info (0))
#endif /* _STDARG_H */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#define __va_round(AP,TYPE)					\
  ((AP).__va_ap = ((AP).__va_ap + __alignof__ (TYPE) - 1 ) &	\
   ~(__alignof__ (TYPE) - 1))

#define va_arg(AP, TYPE) \
  ((AP).__va_num < 2 && __builtin_classify_type (* (TYPE *)0) < 12	\
   ? (__builtin_classify_type (* (TYPE *)0) == 8		\
      ? (*(TYPE *)&(AP).__va_f[(AP).__va_num++])		\
      : ((TYPE)((AP).__va_r[(AP).__va_num++])))			\
   : ((AP).__va_num++, __va_round (AP,TYPE), *((TYPE *)((AP).__va_ap))++))

#define va_end(AP)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
