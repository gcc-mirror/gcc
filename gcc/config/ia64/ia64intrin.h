#ifndef _IA64INTRIN_H_INCLUDED
#define _IA64INTRIN_H_INCLUDED

void __sync_synchronize (void);

int  __sync_val_compare_and_swap_si (int *, int, int);
long __sync_val_compare_and_swap_di (long *, long, long);
#define __sync_val_compare_and_swap(A,B,C) ((sizeof (*(A)) == sizeof(int)) ? __sync_val_compare_and_swap_si((int *)(A),(int)(B),(int)(C)) : __sync_val_compare_and_swap_di((long *)(A),(long)(B),(long)(C)))

int  __sync_bool_compare_and_swap_si (int *, int, int);
long __sync_bool_compare_and_swap_di (long *, long, long);
#define __sync_bool_compare_and_swap(A,B,C) ((sizeof (*(A)) == sizeof(int)) ? __sync_bool_compare_and_swap_si((int *)(A),(int)(B),(int)(C)) : __sync_bool_compare_and_swap_di((long *)(A),(long)(B),(long)(C)))

void __sync_lock_release_si (int *);
void __sync_lock_release_di (long *);
#define __sync_lock_release(A) ((sizeof (*(A)) == sizeof(int)) ? __sync_lock_release_si((int *)(A)) : __sync_lock_release_di((long *)(A)))

int  __sync_lock_test_and_set_si (int *, int);
long __sync_lock_test_and_set_di (long *, long);
#define __sync_lock_test_and_set(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_lock_test_and_set_si((int *)(A),(int)(B)) : __sync_lock_test_and_set_di((long *)(A),(long)(B)))

int  __sync_fetch_and_add_si  (int *, int);
int  __sync_fetch_and_sub_si  (int *, int);
int  __sync_fetch_and_and_si  (int *, int);
int  __sync_fetch_and_or_si   (int *, int);
int  __sync_fetch_and_xor_si  (int *, int);
int  __sync_fetch_and_nand_si (int *, int);
long __sync_fetch_and_add_di  (long *, long);
long __sync_fetch_and_sub_di  (long *, long);
long __sync_fetch_and_and_di  (long *, long);
long __sync_fetch_and_or_di   (long *, long);
long __sync_fetch_and_xor_di  (long *, long);
long __sync_fetch_and_nand_di (long *, long);
#define __sync_fetch_and_add(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_add_si((int *)(A),(int)(B)) : __sync_fetch_and_add_di((long *)(A),(long)(B)))
#define __sync_fetch_and_sub(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_sub_si((int *)(A),(int)(B)) : __sync_fetch_and_sub_di((long *)(A),(long)(B)))
#define __sync_fetch_and_and(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_and_si((int *)(A),(int)(B)) : __sync_fetch_and_and_di((long *)(A),(long)(B)))
#define __sync_fetch_and_or(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_or_si((int *)(A),(int)(B)) : __sync_fetch_and_or_di((long *)(A),(long)(B)))
#define __sync_fetch_and_xor(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_xor_si((int *)(A),(int)(B)) : __sync_fetch_and_xor_di((long *)(A),(long)(B)))
#define __sync_fetch_and_nand(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_fetch_and_nand_si((int *)(A),(int)(B)) : __sync_fetch_and_nand_di((long *)(A),(long)(B)))

int  __sync_add_and_fetch_si  (int *, int);
int  __sync_sub_and_fetch_si  (int *, int);
int  __sync_and_and_fetch_si  (int *, int);
int  __sync_or_and_fetch_si   (int *, int);
int  __sync_xor_and_fetch_si  (int *, int);
int  __sync_nand_and_fetch_si (int *, int);
long __sync_add_and_fetch_di  (long *, long);
long __sync_sub_and_fetch_di  (long *, long);
long __sync_and_and_fetch_di  (long *, long);
long __sync_or_and_fetch_di   (long *, long);
long __sync_xor_and_fetch_di  (long *, long);
long __sync_nand_and_fetch_di (long *, long);
#define __sync_add_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_add_and_fetch_si((int *)(A),(int)(B)) : __sync_add_and_fetch_di((long *)(A),(long)(B)))
#define __sync_sub_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_sub_and_fetch_si((int *)(A),(int)(B)) : __sync_sub_and_fetch_di((long *)(A),(long)(B)))
#define __sync_and_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_and_and_fetch_si((int *)(A),(int)(B)) : __sync_and_and_fetch_di((long *)(A),(long)(B)))
#define __sync_or_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_or_and_fetch_si((int *)(A),(int)(B)) : __sync_or_and_fetch_di((long *)(A),(long)(B)))
#define __sync_xor_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_xor_and_fetch_si((int *)(A),(int)(B)) : __sync_xor_and_fetch_di((long *)(A),(long)(B)))
#define __sync_nand_and_fetch(A,B) ((sizeof (*(A)) == sizeof(int)) ? __sync_nand_and_fetch_si((int *)(A),(int)(B)) : __sync_nand_and_fetch_di((long *)(A),(long)(B)))

#endif
