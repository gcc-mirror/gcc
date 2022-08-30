/* { dg-do compile } */
/* { dg-options "-Og -Wuninitialized -ftree-bit-ccp -fno-thread-jumps -fdump-tree-uninit2" } */

void exit(int __status) __attribute__ ((__noreturn__));
int posix_memalign(void **__memptr, __SIZE_TYPE__ __alignment,
		   __SIZE_TYPE__ __size);

void *f(void)
{
  void *ptr;

  if (posix_memalign(&ptr, 16, 256) != 0)
    exit(1);

  return ptr; /* { dg-bogus "uninitialized" } */
}

/* Make sure the uninit pass has something to do, add to the set of
   disabled optimizations if not.  */
/* { dg-final { scan-tree-dump "# ptr_. = PHI" "uninit2" } } */
