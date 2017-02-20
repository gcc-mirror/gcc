/* Test strncmp builtin expansion for compilation and proper execution.  */
/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ptr32plus } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>

int lib_memcmp(const void *a, const void *b, size_t n) asm("memcmp");
int lib_strncmp(const char *a, const char *b, size_t n) asm("strncmp");

static void test_driver_strncmp (void (test_strncmp)(const char *, const char *, int),
				 void (test_memcmp)(const void *, const void *, int),
				 size_t sz)
{
  long pgsz = sysconf(_SC_PAGESIZE);
  char buf1[sz+1];
  char *buf2;
#if _POSIX_C_SOURCE >= 200112L
  if ( posix_memalign ((void **)&buf2, pgsz, 2*pgsz) ) abort ();
#else
  if ( !(buf2 = valloc(2*pgsz))) abort ();
#endif
  char *p2;
  int r,i,e;

  r = mprotect (buf2+pgsz,pgsz,PROT_NONE);
  if (r < 0) abort();
  
  memset(buf1,'A',sz);
  for(i=10; i>=0; i--) {
    p2 = buf2+pgsz-sz-i;
    memset(p2,'A',sz);
    e = lib_strncmp(buf1,p2,sz);
    (*test_strncmp)(buf1,p2,e);
    e = lib_memcmp(buf1,p2,sz);
    (*test_memcmp)(buf1,p2,e);
  }
  free(buf2);
}

#define RUN_TEST(SZ) test_driver_strncmp (test_strncmp_ ## SZ, test_memcmp_ ## SZ, SZ);

#define DEF_TEST(SZ) \
__attribute__((noinline))						  \
void test_strncmp_ ## SZ (const char *str1, const char *str2, int expect) \
{									  \
  int r;								  \
  r = strncmp(str1,str2,SZ);						  \
  if ( r < 0 && !(expect < 0) ) abort();				  \
  if ( r > 0 && !(expect > 0) )	abort();				  \
  if ( r == 0 && !(expect == 0) ) abort();				  \
}                                                                         \
__attribute__((noinline))						  \
void test_memcmp_ ## SZ (const void *p1, const void *p2, int expect)      \
{									  \
  int r;								  \
  r = memcmp(p1,p2,SZ);						          \
  if ( r < 0 && !(expect < 0) ) abort();				  \
  if ( r > 0 && !(expect > 0) )	abort();				  \
  if ( r == 0 && !(expect == 0) ) abort();				  \
}

DEF_TEST(1)
DEF_TEST(2)
DEF_TEST(3)
DEF_TEST(4)
DEF_TEST(5)
DEF_TEST(6)
DEF_TEST(7)
DEF_TEST(8)
DEF_TEST(9)
DEF_TEST(10)
DEF_TEST(11)
DEF_TEST(12)
DEF_TEST(13)
DEF_TEST(14)
DEF_TEST(15)
DEF_TEST(16)

int
main(int argc, char **argv)
{
  RUN_TEST(1) ;
  RUN_TEST(2) ;
  RUN_TEST(3) ;
  RUN_TEST(4) ;
  RUN_TEST(5) ;
  RUN_TEST(6) ;
  RUN_TEST(7) ;
  RUN_TEST(8) ;
  RUN_TEST(9) ;
  RUN_TEST(10);
  RUN_TEST(11);
  RUN_TEST(12);
  RUN_TEST(13);
  RUN_TEST(14);
  RUN_TEST(15);
  RUN_TEST(16);
  return 0;
}
