/* GNU C varargs and stdargs support for Clipper.  */

/* Define __gnuc_va_list. */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef struct
{
  int __va_ap;				/* pointer to stack args */
  void *__va_reg[4];			/* pointer to r0,f0,r1,f1 */
  int __va_num;				/* number of args processed */
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */


#if defined (_STDARG_H) || defined (_VARARGS_H)
#define va_list __gnuc_va_list
#define __va_list __gnuc_va_list	/* acc compatibility */

#define _VA_LIST
#define _VA_LIST_
#define _SYS_INT_STDARG_H		/* acc compatibility */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that LASTARG is correct.  */
#ifdef _STDARG_H
#define va_start(AP,LASTARG)			\
  (__builtin_next_arg (LASTARG),		\
   (AP) = *(va_list *)__builtin_saveregs(),	\
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

/* round to alignment of `type' but keep a least integer alignment */
#define __va_round(AP,TYPE)					\
  ((AP).__va_ap = ((AP).__va_ap + __alignof__ (TYPE) - 1 ) &	\
   ~(__alignof__ (TYPE) - 1),					\
  ((AP).__va_ap = ((AP).__va_ap + sizeof (int) - 1) & ~(sizeof (int) - 1)))

#define va_arg(AP, TYPE) \
  (*((AP).__va_num < 2 && __builtin_classify_type (* (TYPE *)0) < 12	\
   ? (__builtin_classify_type (* (TYPE *)0) == 8			\
      ? ((TYPE *)(AP).__va_reg[2 * (AP).__va_num++ + 1])		\
      : ((TYPE *)(AP).__va_reg[2 * (AP).__va_num++ ]))			\
   : ((AP).__va_num++, __va_round (AP,TYPE), ((TYPE *)((AP).__va_ap))++)))

#define va_end(AP)	((void) 0)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
