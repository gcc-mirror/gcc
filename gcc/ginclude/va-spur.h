/*  varargs.h for SPUR */

/* NB.  This is NOT the definition needed for the new ANSI proposed
   standard */
 

struct __va_struct { char __regs[20]; };

#define va_alist __va_regs, __va_stack

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl struct __va_struct __va_regs; int __va_stack; 

typedef struct {
    int __pnt;
    char *__regs;
    char *__stack;
} va_list;

#define va_start(pvar) \
     ((pvar).__pnt = 0, (pvar).__regs = __va_regs.__regs, \
      (pvar).__stack = (char *) &__va_stack)
#define va_end(pvar)

/* Avoid errors if compiling GCC v2 with GCC v1.  */
#if __GNUC__ == 1
#define __extension__
#endif

#define va_arg(pvar,type)  \
__extension__ \
    ({  type __va_result; \
        if ((pvar).__pnt >= 20) { \
           __va_result = *( (type *) ((pvar).__stack + (pvar).__pnt - 20)); \
	   (pvar).__pnt += (sizeof(type) + 7) & ~7; \
	} \
	else if ((pvar).__pnt + sizeof(type) > 20) { \
	   __va_result = * (type *) (pvar).__stack; \
	   (pvar).__pnt = 20 + ( (sizeof(type) + 7) & ~7); \
	} \
	else if (sizeof(type) == 8) { \
	   union {double d; int i[2];} __u; \
	   __u.i[0] = *(int *) ((pvar).__regs + (pvar).__pnt); \
	   __u.i[1] = *(int *) ((pvar).__regs + (pvar).__pnt + 4); \
	   __va_result = * (type *) &__u; \
	   (pvar).__pnt += 8; \
	} \
	else { \
	   __va_result = * (type *) ((pvar).__regs + (pvar).__pnt); \
	   (pvar).__pnt += (sizeof(type) + 3) & ~3; \
	} \
	__va_result; })
