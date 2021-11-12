/* Shared header for testcases for copy_from_user/copy_to_user.  */

/* Adapted from include/linux/compiler.h  */

#define __user

/* Adapted from include/asm-generic/uaccess.h  */

extern int copy_from_user(void *to, const void __user *from, long n)
  __attribute__((access (write_only, 1, 3),
		 access (read_only, 2, 3)));

extern long copy_to_user(void __user *to, const void *from, unsigned long n)
  __attribute__((access (write_only, 1, 3),
		 access (read_only, 2, 3)));
