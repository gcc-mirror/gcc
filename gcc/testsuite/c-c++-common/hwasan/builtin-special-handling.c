/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-asan" } */
/* { dg-skip-if "" { *-*-* }  { "-O0" } { "" } } */
/* Only skip the -flto tests without the -flto-partition=none.
   With -flto-partition=none we still get a asan1 dump file, without that
   parameter we only get the lto dump files (which means scan-tree-dump-times
   doesn't work.  */
/* { dg-skip-if "" { *-*-* }  { "-flto" } { "-flto-partition=none" } } */

typedef __SIZE_TYPE__ size_t;
/* HWASAN used to instrument calls to memset, memcpy, and memmove.  It no
   longer does this.  Many other string and memory builtins are intercepted by
   the runtime (and hence the codegen need not do anything).  */
void * __attribute__((noinline))
memset_builtin (void *dest, int value, size_t len)
{
  return __builtin_memset (dest, value, len);
}

size_t __attribute__ ((noinline))
strlen_builtin (char *element)
{
  return __builtin_strlen (element);
}

/* First check here ensures there is no inline instrumentation generated for
   these builtins.  Second checks that we end up calling memset (i.e. that it's
   not optimised into an inline operation, which would happen without the
   instrumentation).  */
/* { dg-final { scan-tree-dump-not "HWASAN_CHECK" "asan1" } } */
/* { dg-final { scan-assembler-times "\tmemset\\M" 1 } } */
