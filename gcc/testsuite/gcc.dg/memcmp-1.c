/* Test memcmp/strncmp builtin expansion for compilation and proper execution.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ptr32plus } */
/* { dg-additional-options "-DRUN_FRACTION=7" { target simulator } } */
/* { dg-timeout-factor 2 } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define STR1(X) #X
#define STR2(X) STR1(X)

int lib_memcmp(const void *a, const void *b, size_t n)
 asm(STR2(__USER_LABEL_PREFIX__) "memcmp");
int lib_strncmp(const char *a, const char *b, size_t n)
 asm(STR2(__USER_LABEL_PREFIX__) "strncmp");

#ifndef NRAND
#ifdef TEST_ALL
#define NRAND 10000
#else
#define NRAND 500
#endif
#endif
#ifndef TZONE
#ifdef TEST_ALL
#define TZONE 16
#else
#define TZONE 8
#endif
#endif

#define MAX_SZ 600

/* A means to run only a fraction of the tests, beginning at a random
   count.  */
#ifdef RUN_FRACTION

#define SKIP_ITERATION skip_iteration ()
static unsigned int iteration_count;

static _Bool
skip_iteration (void)
{
  _Bool run = ++iteration_count == RUN_FRACTION;

  if (run)
    iteration_count = 0;

  return !run;
}

static void
initialize_skip_iteration_count ()
{
  srand (2024);
  iteration_count = (unsigned int) (rand ()) % RUN_FRACTION;
}

#else
#define SKIP_ITERATION 0
#define initialize_skip_iteration_count()
#endif

#define DEF_RS(ALIGN)                                                      \
static void test_memcmp_runtime_size_ ## ALIGN (const char *str1, 	   \
						const char *str2,	   \
						size_t sz, int expect)	   \
{									   \
  char three[8192] __attribute__ ((aligned (4096)));			   \
  char four[8192] __attribute__ ((aligned (4096)));			   \
  char *a, *b;								   \
  int i,j,a1,a2,r;							   \
  for (j = 0; j < 2; j++)						   \
    {									   \
      for (i = 0; i < 2; i++)						   \
	{								   \
	  a = three+i*ALIGN+j*(4096-2*i*ALIGN);				   \
	  b = four+i*ALIGN+j*(4096-2*i*ALIGN);				   \
	  memcpy(a,str1,sz);						   \
	  memcpy(b,str2,sz);						   \
	  r = memcmp(a,b,sz);						   \
	  if ( r < 0 && !(expect < 0) ) abort();			   \
	  if ( r > 0 && !(expect > 0) )	abort();			   \
	  if ( r == 0 && !(expect == 0) ) abort();			   \
	}								   \
    }									   \
}

DEF_RS(1)
DEF_RS(2)
DEF_RS(4)
DEF_RS(8)
DEF_RS(16)

static void test_memcmp_runtime_size (const char *str1, const char *str2,
				      size_t sz, int expect)
{
  char three[8192] __attribute__ ((aligned (4096)));
  char four[8192] __attribute__ ((aligned (4096)));
  char *a, *b;
  int i,j,a1,a2,r;
  test_memcmp_runtime_size_1 (str1,str2,sz,expect);
  test_memcmp_runtime_size_2 (str1,str2,sz,expect);
  test_memcmp_runtime_size_4 (str1,str2,sz,expect);
  test_memcmp_runtime_size_8 (str1,str2,sz,expect);
  test_memcmp_runtime_size_16 (str1,str2,sz,expect);
  for (j = 0; j < 2; j++)
    {
      for (i = 0; i < 2; i++)
	{
	  for (a1=0; a1 < 2*sizeof(void *); a1++)
	    {
	      a = three+i*a1+j*(4096-2*i*a1);
	      memcpy(a,str1,sz);
	      for (a2=0; a2 < 2*sizeof(void *); a2++)
		{
		  b = four+i*a2+j*(4096-2*i*a2);
		  memcpy(b,str2,sz);
		  r = memcmp(a,b,sz);
		  if ( r < 0 && !(expect < 0) ) abort();
		  if ( r > 0 && !(expect > 0) )	abort();
		  if ( r == 0 && !(expect == 0) ) abort();
		}
	    }
	}
    }
}

static void test_driver_memcmp (void (test_memcmp)(const char *, const char *, int),
				void (test_strncmp)(const char *, const char *, int),
  size_t sz, int align)
{
  char buf1[MAX_SZ*2+TZONE],buf2[MAX_SZ*2+TZONE];
  size_t test_sz = (sz<MAX_SZ)?sz:MAX_SZ;
  size_t diff_pos, zero_pos;
  uint32_t e;
  int i,j,l;
  for(l=0;l<sz;l++) {
    for(i=0;i<NRAND/sz;i++) {
      if (SKIP_ITERATION)
	continue;
      for(j=0;j<l;j++) {
	buf1[j] = rand() & 0xff;
	buf2[j] = buf1[j];
      }
      for(j=l;j<sz;j++) {
	buf1[j] = rand() & 0xff;
	buf2[j] = rand() & 0xff;
      }
      e = lib_memcmp(buf1,buf2,sz);
      (*test_memcmp)(buf1,buf2,e);
      test_memcmp_runtime_size (buf1, buf2, sz, e);
      e = lib_strncmp(buf1,buf2,sz);
      (*test_strncmp)(buf1,buf2,e);
    }
  }
  for(diff_pos = ((test_sz>TZONE)?(test_sz-TZONE):0); diff_pos < test_sz+TZONE; diff_pos++)
    for(zero_pos = ((test_sz>TZONE)?(test_sz-TZONE):0); zero_pos < test_sz+TZONE; zero_pos++)
      {
	if (SKIP_ITERATION)
	  continue;
	memset(buf1, 'A', 2*test_sz);
	memset(buf2, 'A', 2*test_sz);
	buf2[diff_pos] = 'B';
	buf1[zero_pos] = 0;
	buf2[zero_pos] = 0;
	e = lib_memcmp(buf1,buf2,sz);
	(*test_memcmp)(buf1,buf2,e);
	(*test_memcmp)(buf2,buf1,-e);
	(*test_memcmp)(buf2,buf2,0);
	test_memcmp_runtime_size (buf1, buf2, sz, e);
	test_memcmp_runtime_size (buf2, buf1, sz, -e);
	e = lib_strncmp(buf1,buf2,sz);
	(*test_strncmp)(buf1,buf2,e);
	(*test_strncmp)(buf2,buf1,-e);
	(*test_strncmp)(buf2,buf2,0);
	/* differing length: */
	buf2[diff_pos] = 0;
	e = lib_memcmp(buf1,buf2,sz);
	(*test_memcmp)(buf1,buf2,e);
	test_memcmp_runtime_size (buf1, buf2, sz, e);
	e = lib_strncmp(buf1,buf2,sz);
	(*test_strncmp)(buf1,buf2,e);
	memset(buf2+diff_pos,'B',sizeof(buf2)-diff_pos);
	buf2[zero_pos] = 0;
	e = lib_memcmp(buf1,buf2,sz);
	(*test_memcmp)(buf1,buf2,e);
	(*test_memcmp)(buf2,buf1,-e);
	test_memcmp_runtime_size (buf1, buf2, sz, e);
	test_memcmp_runtime_size (buf2, buf1, sz, -e);
	e = lib_strncmp(buf1,buf2,sz);
	(*test_strncmp)(buf1,buf2,e);
	(*test_strncmp)(buf2,buf1,-e);
      }
}

#define RUN_TEST(SZ, ALIGN) test_driver_memcmp (test_memcmp_ ## SZ ## _ ## ALIGN, test_strncmp_ ## SZ ## _ ## ALIGN, SZ, ALIGN);

#define DEF_TEST(SZ, ALIGN)						\
  static void test_memcmp_ ## SZ ## _ ## ALIGN (const char *str1, const char *str2, int expect)	\
{									\
  char three[8192] __attribute__ ((aligned (4096)));			\
  char four[8192] __attribute__ ((aligned (4096)));			\
  char *a, *b;								\
  int i,j,r;								\
  for (j = 0; j < 2; j++)						\
    {									\
      for (i = 0; i < 2; i++)						\
	{								\
	  a = three+i*ALIGN+j*(4096-2*i*ALIGN);				\
	  b = four+i*ALIGN+j*(4096-2*i*ALIGN);				\
	  memcpy(a,str1,SZ);						\
	  memcpy(b,str2,SZ);						\
	  r = memcmp(a,b,SZ);						\
	  if ( r < 0 && !(expect < 0) ) abort();			\
	  if ( r > 0 && !(expect > 0) )	abort();			\
	  if ( r == 0 && !(expect == 0) ) abort();			\
	}								\
    }									\
}									\
static void test_strncmp_ ## SZ ## _ ## ALIGN (const char *str1, const char *str2, int expect)	 \
{									\
  char three[8192] __attribute__ ((aligned (4096)));			\
  char four[8192] __attribute__ ((aligned (4096)));			\
  char *a, *b;								\
  int i,j,r;								\
  for (j = 0; j < 2; j++)						\
    {									\
      for (i = 0; i < 2; i++)						\
	{								\
	  a = three+i*ALIGN+j*(4096-2*i*ALIGN);				\
	  b = four+i*ALIGN+j*(4096-2*i*ALIGN);				\
	  memcpy(a,str1,SZ);						\
	  memcpy(b,str2,SZ);						\
	  r = strncmp(a,b,SZ);						\
	  if ( r < 0 && !(expect < 0) ) abort();			\
	  if ( r > 0 && !(expect > 0) )	abort();			\
	  if ( r == 0 && !(expect == 0) ) abort();			\
	}								\
    }									\
}

#ifdef TEST_ALL
DEF_TEST(1,1)
DEF_TEST(1,2)
DEF_TEST(1,4)
DEF_TEST(1,8)
DEF_TEST(1,16)
DEF_TEST(2,1)
DEF_TEST(2,2)
DEF_TEST(2,4)
DEF_TEST(2,8)
DEF_TEST(2,16)
DEF_TEST(3,1)
DEF_TEST(3,2)
DEF_TEST(3,4)
DEF_TEST(3,8)
DEF_TEST(3,16)
DEF_TEST(4,1)
DEF_TEST(4,2)
DEF_TEST(4,4)
DEF_TEST(4,8)
DEF_TEST(4,16)
DEF_TEST(5,1)
DEF_TEST(5,2)
DEF_TEST(5,4)
DEF_TEST(5,8)
DEF_TEST(5,16)
DEF_TEST(6,1)
DEF_TEST(6,2)
DEF_TEST(6,4)
DEF_TEST(6,8)
DEF_TEST(6,16)
DEF_TEST(7,1)
DEF_TEST(7,2)
DEF_TEST(7,4)
DEF_TEST(7,8)
DEF_TEST(7,16)
DEF_TEST(8,1)
DEF_TEST(8,2)
DEF_TEST(8,4)
DEF_TEST(8,8)
DEF_TEST(8,16)
DEF_TEST(9,1)
DEF_TEST(9,2)
DEF_TEST(9,4)
DEF_TEST(9,8)
DEF_TEST(9,16)
DEF_TEST(10,1)
DEF_TEST(10,2)
DEF_TEST(10,4)
DEF_TEST(10,8)
DEF_TEST(10,16)
DEF_TEST(11,1)
DEF_TEST(11,2)
DEF_TEST(11,4)
DEF_TEST(11,8)
DEF_TEST(11,16)
DEF_TEST(12,1)
DEF_TEST(12,2)
DEF_TEST(12,4)
DEF_TEST(12,8)
DEF_TEST(12,16)
DEF_TEST(13,1)
DEF_TEST(13,2)
DEF_TEST(13,4)
DEF_TEST(13,8)
DEF_TEST(13,16)
DEF_TEST(14,1)
DEF_TEST(14,2)
DEF_TEST(14,4)
DEF_TEST(14,8)
DEF_TEST(14,16)
DEF_TEST(15,1)
DEF_TEST(15,2)
DEF_TEST(15,4)
DEF_TEST(15,8)
DEF_TEST(15,16)
DEF_TEST(16,1)
DEF_TEST(16,2)
DEF_TEST(16,4)
DEF_TEST(16,8)
DEF_TEST(16,16)
DEF_TEST(17,1)
DEF_TEST(17,2)
DEF_TEST(17,4)
DEF_TEST(17,8)
DEF_TEST(17,16)
DEF_TEST(18,1)
DEF_TEST(18,2)
DEF_TEST(18,4)
DEF_TEST(18,8)
DEF_TEST(18,16)
DEF_TEST(19,1)
DEF_TEST(19,2)
DEF_TEST(19,4)
DEF_TEST(19,8)
DEF_TEST(19,16)
DEF_TEST(20,1)
DEF_TEST(20,2)
DEF_TEST(20,4)
DEF_TEST(20,8)
DEF_TEST(20,16)
DEF_TEST(21,1)
DEF_TEST(21,2)
DEF_TEST(21,4)
DEF_TEST(21,8)
DEF_TEST(21,16)
DEF_TEST(22,1)
DEF_TEST(22,2)
DEF_TEST(22,4)
DEF_TEST(22,8)
DEF_TEST(22,16)
DEF_TEST(23,1)
DEF_TEST(23,2)
DEF_TEST(23,4)
DEF_TEST(23,8)
DEF_TEST(23,16)
DEF_TEST(24,1)
DEF_TEST(24,2)
DEF_TEST(24,4)
DEF_TEST(24,8)
DEF_TEST(24,16)
DEF_TEST(25,1)
DEF_TEST(25,2)
DEF_TEST(25,4)
DEF_TEST(25,8)
DEF_TEST(25,16)
DEF_TEST(26,1)
DEF_TEST(26,2)
DEF_TEST(26,4)
DEF_TEST(26,8)
DEF_TEST(26,16)
DEF_TEST(27,1)
DEF_TEST(27,2)
DEF_TEST(27,4)
DEF_TEST(27,8)
DEF_TEST(27,16)
DEF_TEST(28,1)
DEF_TEST(28,2)
DEF_TEST(28,4)
DEF_TEST(28,8)
DEF_TEST(28,16)
DEF_TEST(29,1)
DEF_TEST(29,2)
DEF_TEST(29,4)
DEF_TEST(29,8)
DEF_TEST(29,16)
DEF_TEST(30,1)
DEF_TEST(30,2)
DEF_TEST(30,4)
DEF_TEST(30,8)
DEF_TEST(30,16)
DEF_TEST(31,1)
DEF_TEST(31,2)
DEF_TEST(31,4)
DEF_TEST(31,8)
DEF_TEST(31,16)
DEF_TEST(32,1)
DEF_TEST(32,2)
DEF_TEST(32,4)
DEF_TEST(32,8)
DEF_TEST(32,16)
DEF_TEST(33,1)
DEF_TEST(33,2)
DEF_TEST(33,4)
DEF_TEST(33,8)
DEF_TEST(33,16)
DEF_TEST(34,1)
DEF_TEST(34,2)
DEF_TEST(34,4)
DEF_TEST(34,8)
DEF_TEST(34,16)
DEF_TEST(35,1)
DEF_TEST(35,2)
DEF_TEST(35,4)
DEF_TEST(35,8)
DEF_TEST(35,16)
DEF_TEST(36,1)
DEF_TEST(36,2)
DEF_TEST(36,4)
DEF_TEST(36,8)
DEF_TEST(36,16)
DEF_TEST(37,1)
DEF_TEST(37,2)
DEF_TEST(37,4)
DEF_TEST(37,8)
DEF_TEST(37,16)
DEF_TEST(38,1)
DEF_TEST(38,2)
DEF_TEST(38,4)
DEF_TEST(38,8)
DEF_TEST(38,16)
DEF_TEST(39,1)
DEF_TEST(39,2)
DEF_TEST(39,4)
DEF_TEST(39,8)
DEF_TEST(39,16)
DEF_TEST(40,1)
DEF_TEST(40,2)
DEF_TEST(40,4)
DEF_TEST(40,8)
DEF_TEST(40,16)
DEF_TEST(41,1)
DEF_TEST(41,2)
DEF_TEST(41,4)
DEF_TEST(41,8)
DEF_TEST(41,16)
DEF_TEST(42,1)
DEF_TEST(42,2)
DEF_TEST(42,4)
DEF_TEST(42,8)
DEF_TEST(42,16)
DEF_TEST(43,1)
DEF_TEST(43,2)
DEF_TEST(43,4)
DEF_TEST(43,8)
DEF_TEST(43,16)
DEF_TEST(44,1)
DEF_TEST(44,2)
DEF_TEST(44,4)
DEF_TEST(44,8)
DEF_TEST(44,16)
DEF_TEST(45,1)
DEF_TEST(45,2)
DEF_TEST(45,4)
DEF_TEST(45,8)
DEF_TEST(45,16)
DEF_TEST(46,1)
DEF_TEST(46,2)
DEF_TEST(46,4)
DEF_TEST(46,8)
DEF_TEST(46,16)
DEF_TEST(47,1)
DEF_TEST(47,2)
DEF_TEST(47,4)
DEF_TEST(47,8)
DEF_TEST(47,16)
DEF_TEST(48,1)
DEF_TEST(48,2)
DEF_TEST(48,4)
DEF_TEST(48,8)
DEF_TEST(48,16)
DEF_TEST(49,1)
DEF_TEST(49,2)
DEF_TEST(49,4)
DEF_TEST(49,8)
DEF_TEST(49,16)
DEF_TEST(100,1)
DEF_TEST(100,2)
DEF_TEST(100,4)
DEF_TEST(100,8)
DEF_TEST(100,16)
DEF_TEST(191,1)
DEF_TEST(192,1)
DEF_TEST(193,1)
DEF_TEST(200,1)
DEF_TEST(400,1)
#else
DEF_TEST(1,1)
DEF_TEST(2,1)
DEF_TEST(3,1)
DEF_TEST(4,1)
DEF_TEST(5,1)
DEF_TEST(5,8)
DEF_TEST(6,1)
DEF_TEST(6,4)
DEF_TEST(6,8)
DEF_TEST(7,1)
DEF_TEST(7,2)
DEF_TEST(7,4)
DEF_TEST(7,8)
DEF_TEST(8,1)
DEF_TEST(9,1)
DEF_TEST(16,1)
DEF_TEST(32,1)
DEF_TEST(33,8)
DEF_TEST(49,1)
#endif

int
main(int argc, char **argv)
{
  initialize_skip_iteration_count ();
#ifdef TEST_ALL
    RUN_TEST(1,1)
    RUN_TEST(1,2)
    RUN_TEST(1,4)
    RUN_TEST(1,8)
    RUN_TEST(1,16)
    RUN_TEST(2,1)
    RUN_TEST(2,2)
    RUN_TEST(2,4)
    RUN_TEST(2,8)
    RUN_TEST(2,16)
    RUN_TEST(3,1)
    RUN_TEST(3,2)
    RUN_TEST(3,4)
    RUN_TEST(3,8)
    RUN_TEST(3,16)
    RUN_TEST(4,1)
    RUN_TEST(4,2)
    RUN_TEST(4,4)
    RUN_TEST(4,8)
    RUN_TEST(4,16)
    RUN_TEST(5,1)
    RUN_TEST(5,2)
    RUN_TEST(5,4)
    RUN_TEST(5,8)
    RUN_TEST(5,16)
    RUN_TEST(6,1)
    RUN_TEST(6,2)
    RUN_TEST(6,4)
    RUN_TEST(6,8)
    RUN_TEST(6,16)
    RUN_TEST(7,1)
    RUN_TEST(7,2)
    RUN_TEST(7,4)
    RUN_TEST(7,8)
    RUN_TEST(7,16)
    RUN_TEST(8,1)
    RUN_TEST(8,2)
    RUN_TEST(8,4)
    RUN_TEST(8,8)
    RUN_TEST(8,16)
    RUN_TEST(9,1)
    RUN_TEST(9,2)
    RUN_TEST(9,4)
    RUN_TEST(9,8)
    RUN_TEST(9,16)
    RUN_TEST(10,1)
    RUN_TEST(10,2)
    RUN_TEST(10,4)
    RUN_TEST(10,8)
    RUN_TEST(10,16)
    RUN_TEST(11,1)
    RUN_TEST(11,2)
    RUN_TEST(11,4)
    RUN_TEST(11,8)
    RUN_TEST(11,16)
    RUN_TEST(12,1)
    RUN_TEST(12,2)
    RUN_TEST(12,4)
    RUN_TEST(12,8)
    RUN_TEST(12,16)
    RUN_TEST(13,1)
    RUN_TEST(13,2)
    RUN_TEST(13,4)
    RUN_TEST(13,8)
    RUN_TEST(13,16)
    RUN_TEST(14,1)
    RUN_TEST(14,2)
    RUN_TEST(14,4)
    RUN_TEST(14,8)
    RUN_TEST(14,16)
    RUN_TEST(15,1)
    RUN_TEST(15,2)
    RUN_TEST(15,4)
    RUN_TEST(15,8)
    RUN_TEST(15,16)
    RUN_TEST(16,1)
    RUN_TEST(16,2)
    RUN_TEST(16,4)
    RUN_TEST(16,8)
    RUN_TEST(16,16)
    RUN_TEST(17,1)
    RUN_TEST(17,2)
    RUN_TEST(17,4)
    RUN_TEST(17,8)
    RUN_TEST(17,16)
    RUN_TEST(18,1)
    RUN_TEST(18,2)
    RUN_TEST(18,4)
    RUN_TEST(18,8)
    RUN_TEST(18,16)
    RUN_TEST(19,1)
    RUN_TEST(19,2)
    RUN_TEST(19,4)
    RUN_TEST(19,8)
    RUN_TEST(19,16)
    RUN_TEST(20,1)
    RUN_TEST(20,2)
    RUN_TEST(20,4)
    RUN_TEST(20,8)
    RUN_TEST(20,16)
    RUN_TEST(21,1)
    RUN_TEST(21,2)
    RUN_TEST(21,4)
    RUN_TEST(21,8)
    RUN_TEST(21,16)
    RUN_TEST(22,1)
    RUN_TEST(22,2)
    RUN_TEST(22,4)
    RUN_TEST(22,8)
    RUN_TEST(22,16)
    RUN_TEST(23,1)
    RUN_TEST(23,2)
    RUN_TEST(23,4)
    RUN_TEST(23,8)
    RUN_TEST(23,16)
    RUN_TEST(24,1)
    RUN_TEST(24,2)
    RUN_TEST(24,4)
    RUN_TEST(24,8)
    RUN_TEST(24,16)
    RUN_TEST(25,1)
    RUN_TEST(25,2)
    RUN_TEST(25,4)
    RUN_TEST(25,8)
    RUN_TEST(25,16)
    RUN_TEST(26,1)
    RUN_TEST(26,2)
    RUN_TEST(26,4)
    RUN_TEST(26,8)
    RUN_TEST(26,16)
    RUN_TEST(27,1)
    RUN_TEST(27,2)
    RUN_TEST(27,4)
    RUN_TEST(27,8)
    RUN_TEST(27,16)
    RUN_TEST(28,1)
    RUN_TEST(28,2)
    RUN_TEST(28,4)
    RUN_TEST(28,8)
    RUN_TEST(28,16)
    RUN_TEST(29,1)
    RUN_TEST(29,2)
    RUN_TEST(29,4)
    RUN_TEST(29,8)
    RUN_TEST(29,16)
    RUN_TEST(30,1)
    RUN_TEST(30,2)
    RUN_TEST(30,4)
    RUN_TEST(30,8)
    RUN_TEST(30,16)
    RUN_TEST(31,1)
    RUN_TEST(31,2)
    RUN_TEST(31,4)
    RUN_TEST(31,8)
    RUN_TEST(31,16)
    RUN_TEST(32,1)
    RUN_TEST(32,2)
    RUN_TEST(32,4)
    RUN_TEST(32,8)
    RUN_TEST(32,16)
    RUN_TEST(33,1)
    RUN_TEST(33,2)
    RUN_TEST(33,4)
    RUN_TEST(33,8)
    RUN_TEST(33,16)
    RUN_TEST(34,1)
    RUN_TEST(34,2)
    RUN_TEST(34,4)
    RUN_TEST(34,8)
    RUN_TEST(34,16)
    RUN_TEST(35,1)
    RUN_TEST(35,2)
    RUN_TEST(35,4)
    RUN_TEST(35,8)
    RUN_TEST(35,16)
    RUN_TEST(36,1)
    RUN_TEST(36,2)
    RUN_TEST(36,4)
    RUN_TEST(36,8)
    RUN_TEST(36,16)
    RUN_TEST(37,1)
    RUN_TEST(37,2)
    RUN_TEST(37,4)
    RUN_TEST(37,8)
    RUN_TEST(37,16)
    RUN_TEST(38,1)
    RUN_TEST(38,2)
    RUN_TEST(38,4)
    RUN_TEST(38,8)
    RUN_TEST(38,16)
    RUN_TEST(39,1)
    RUN_TEST(39,2)
    RUN_TEST(39,4)
    RUN_TEST(39,8)
    RUN_TEST(39,16)
    RUN_TEST(40,1)
    RUN_TEST(40,2)
    RUN_TEST(40,4)
    RUN_TEST(40,8)
    RUN_TEST(40,16)
    RUN_TEST(41,1)
    RUN_TEST(41,2)
    RUN_TEST(41,4)
    RUN_TEST(41,8)
    RUN_TEST(41,16)
    RUN_TEST(42,1)
    RUN_TEST(42,2)
    RUN_TEST(42,4)
    RUN_TEST(42,8)
    RUN_TEST(42,16)
    RUN_TEST(43,1)
    RUN_TEST(43,2)
    RUN_TEST(43,4)
    RUN_TEST(43,8)
    RUN_TEST(43,16)
    RUN_TEST(44,1)
    RUN_TEST(44,2)
    RUN_TEST(44,4)
    RUN_TEST(44,8)
    RUN_TEST(44,16)
    RUN_TEST(45,1)
    RUN_TEST(45,2)
    RUN_TEST(45,4)
    RUN_TEST(45,8)
    RUN_TEST(45,16)
    RUN_TEST(46,1)
    RUN_TEST(46,2)
    RUN_TEST(46,4)
    RUN_TEST(46,8)
    RUN_TEST(46,16)
    RUN_TEST(47,1)
    RUN_TEST(47,2)
    RUN_TEST(47,4)
    RUN_TEST(47,8)
    RUN_TEST(47,16)
    RUN_TEST(48,1)
    RUN_TEST(48,2)
    RUN_TEST(48,4)
    RUN_TEST(48,8)
    RUN_TEST(48,16)
    RUN_TEST(49,1)
    RUN_TEST(49,2)
    RUN_TEST(49,4)
    RUN_TEST(49,8)
    RUN_TEST(49,16)
    RUN_TEST(100,1)
    RUN_TEST(100,2)
    RUN_TEST(100,4)
    RUN_TEST(100,8)
    RUN_TEST(100,16)
    RUN_TEST(191,1)
    RUN_TEST(192,1)
    RUN_TEST(193,1)
    RUN_TEST(200,1)
    RUN_TEST(400,1)
#else
    RUN_TEST(1,1)
    RUN_TEST(2,1)
    RUN_TEST(3,1)
    RUN_TEST(4,1)
    RUN_TEST(5,1)
    RUN_TEST(5,8)
    RUN_TEST(6,1)
    RUN_TEST(6,4)
    RUN_TEST(6,8)
    RUN_TEST(7,1)
    RUN_TEST(7,2)
    RUN_TEST(7,4)
    RUN_TEST(7,8)
    RUN_TEST(8,1)
    RUN_TEST(9,1)
    RUN_TEST(16,1)
    RUN_TEST(32,1)
    RUN_TEST(33,8)
    RUN_TEST(49,1)
#endif
}
