#ifndef _IA64INTRIN_H_INCLUDED
#define _IA64INTRIN_H_INCLUDED

/* Actually, everything is a compiler builtin, but just so
   there's no confusion...  */
#ifdef __cplusplus
extern "C" {
#endif

extern void __sync_synchronize (void);

extern int __sync_val_compare_and_swap_si (int *, int, int);
extern long __sync_val_compare_and_swap_di (long *, long, long);
#define __sync_val_compare_and_swap(PTR, OLD, NEW)			\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR)))						\
    __sync_val_compare_and_swap_si((int *)(PTR),(int)(OLD),(int)(NEW))	\
  : (__typeof__(*(PTR)))						\
    __sync_val_compare_and_swap_di((long *)(PTR),(long)(OLD),(long)(NEW)))

extern int __sync_bool_compare_and_swap_si (int *, int, int);
extern int __sync_bool_compare_and_swap_di (long *, long, long);
#define __sync_bool_compare_and_swap(PTR, OLD, NEW)			\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? __sync_bool_compare_and_swap_si((int *)(PTR),(int)(OLD),(int)(NEW))	\
  : __sync_bool_compare_and_swap_di((long *)(PTR),(long)(OLD),(long)(NEW)))

extern void __sync_lock_release_si (int *);
extern void __sync_lock_release_di (long *);
#define __sync_lock_release(PTR)		\
  ((sizeof (*(PTR)) == sizeof(int))		\
   ? __sync_lock_release_si((int *)(PTR))	\
   : __sync_lock_release_di((long *)(PTR)))

extern int __sync_lock_test_and_set_si (int *, int);
extern long __sync_lock_test_and_set_di (long *, long);
#define __sync_lock_test_and_set(PTR,VAL)				\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_lock_test_and_set_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_lock_test_and_set_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_add_si (int *, int);
extern long __sync_fetch_and_add_di (long *, long);
#define __sync_fetch_and_add(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_add_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_add_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_sub_si (int *, int);
extern long __sync_fetch_and_sub_di (long *, long);
#define __sync_fetch_and_sub(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_sub_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_sub_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_and_si (int *, int);
extern long __sync_fetch_and_and_di (long *, long);
#define __sync_fetch_and_and(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_and_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_and_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_or_si (int *, int);
extern long __sync_fetch_and_or_di (long *, long);
#define __sync_fetch_and_or(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_or_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_or_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_xor_si (int *, int);
extern long __sync_fetch_and_xor_di (long *, long);
#define __sync_fetch_and_xor(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_xor_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_xor_di((long *)(PTR),(long)(VAL)))

extern int __sync_fetch_and_nand_si (int *, int);
extern long __sync_fetch_and_nand_di (long *, long);
#define __sync_fetch_and_nand(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_fetch_and_nand_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_fetch_and_nand_di((long *)(PTR),(long)(VAL)))

extern int __sync_add_and_fetch_si (int *, int);
extern long __sync_add_and_fetch_di (long *, long);
#define __sync_add_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_add_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_add_and_fetch_di((long *)(PTR),(long)(VAL)))

extern int __sync_sub_and_fetch_si (int *, int);
extern long __sync_sub_and_fetch_di (long *, long);
#define __sync_sub_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_sub_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_sub_and_fetch_di((long *)(PTR),(long)(VAL)))

extern int __sync_and_and_fetch_si (int *, int);
extern long __sync_and_and_fetch_di (long *, long);
#define __sync_and_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_and_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_and_and_fetch_di((long *)(PTR),(long)(VAL)))

extern int __sync_or_and_fetch_si (int *, int);
extern long __sync_or_and_fetch_di (long *, long);
#define __sync_or_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_or_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_or_and_fetch_di((long *)(PTR),(long)(VAL)))

extern int __sync_xor_and_fetch_si (int *, int);
extern long __sync_xor_and_fetch_di (long *, long);
#define __sync_xor_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_xor_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_xor_and_fetch_di((long *)(PTR),(long)(VAL)))

extern int __sync_nand_and_fetch_si (int *, int);
extern long __sync_nand_and_fetch_di (long *, long);
#define __sync_nand_and_fetch(PTR,VAL)					\
 ((sizeof (*(PTR)) == sizeof(int))					\
  ? (__typeof__(*(PTR))) __sync_nand_and_fetch_si((int *)(PTR),(int)(VAL)) \
  : (__typeof__(*(PTR))) __sync_nand_and_fetch_di((long *)(PTR),(long)(VAL)))

#ifdef __cplusplus
}
#endif

#endif
