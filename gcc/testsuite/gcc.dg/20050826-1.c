/* Test whether strncmp has not been "optimized" into memcmp
   nor any code with memcmp semantics.  */
/* { dg-do run { target i?86-*-linux* x86_64-*-linux* ia64-*-linux* alpha*-*-linux* powerpc*-*-linux* s390*-*-linux* sparc*-*-linux* i?86-*-darwin* powerpc-*-darwin*} } */
/* { dg-options "-O2" } */
#include <sys/mman.h>
/* Darwin spells this differently */
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
#include <stdlib.h>

    struct Flags {
 int filler[18];
 unsigned int a:14;
 unsigned int b:14;
 unsigned int c:1;
 unsigned int d:1;
 unsigned int e:1;
 unsigned int f:1;
    };
static void __attribute__((noinline)) set (struct Flags *);
static void set (struct Flags *fp)
{
  fp->b = 5;
  fp->d = 1;
}

static int __attribute__((noinline)) bar (int);
static int bar(int x) { return !(x==1); }
int main (void)
{
  char *p = mmap (NULL, 131072, PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  struct Flags *fp;
  if (p == MAP_FAILED)
    return 0;
  if (munmap (p + 65536, 65536) < 0)
    return 0;
  fp = (struct Flags*)(p + 65536 - sizeof(struct Flags));
  set(fp);
  if (fp->b > 0)
    return (bar(fp->d));
  return 1;
}
