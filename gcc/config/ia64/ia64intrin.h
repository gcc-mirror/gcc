#ifndef _IA64INTRIN_H_INCLUDED
#define _IA64INTRIN_H_INCLUDED

/* ??? Overloaded builtins havn't been ported to C++ yet.  */
#ifdef __cplusplus

#define __sync_val_compare_and_swap(PTR, OLD, NEW)			\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR)))						\
    __sync_val_compare_and_swap_4((int *)(void *)(PTR),(int)(OLD),(int)(NEW))	\
  : (__typeof__(*(PTR)))						\
    __sync_val_compare_and_swap_8((long *)(void *)(PTR),(long)(OLD),(long)(NEW)))

#define __sync_bool_compare_and_swap(PTR, OLD, NEW)			\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? __sync_bool_compare_and_swap_4((int *)(void *)(PTR),(int)(OLD),(int)(NEW))	\
  : __sync_bool_compare_and_swap_8((long *)(void *)(PTR),(long)(OLD),(long)(NEW)))

#define __sync_lock_release(PTR)		\
  ((sizeof (*(PTR)) == sizeof(int))		\
   ? __sync_lock_release_4((int *)(void *)(PTR))	\
   : __sync_lock_release_8((long *)(void *)(PTR)))

#define __sync_lock_test_and_set(PTR,VAL)				\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_lock_test_and_set_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_lock_test_and_set_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_add(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_add_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_add_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_sub(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_sub_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_sub_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_and(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_and_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_and_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_or(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_or_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_or_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_xor(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_xor_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_xor_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_fetch_and_nand(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_nand_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_nand_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_add_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_add_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_add_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_sub_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_sub_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_sub_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_and_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_and_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_and_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_or_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_or_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_or_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_xor_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_xor_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_xor_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#define __sync_nand_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_nand_and_fetch_4((int *)(void *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_nand_and_fetch_8((long *)(void *)(PTR),(long)(VAL)))

#endif /* __cplusplus */

#endif /* _IA64INTRIN_H_INCLUDED */
