/* PR tree-optimization/89772
   Verify that memchr calls with a pointer to a constant character
   are folded as expected.
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-release_ssa" } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

extern void* memchr (const void*, int, size_t);
extern int printf (const char*, ...);
extern void abort (void);

#define A(expr)						\
  ((expr)						\
   ? (void)0						\
   : (printf ("assertion failed on line %i: %s\n",	\
	      __LINE__, #expr),				\
      abort ()))

const char a[8] = {'a',0,'b'};
const char b[3] = {'a','b'};
const char c[8] = {'a','b','c'};

void test_memchr_cst_char (void)
{
  A (!memchr (a, 'c', 2));
  A (!memchr (a, 'c', 5));
  A (!memchr (a, 'c', sizeof a));
  A (&a[1] == memchr (a, 0, sizeof a));

  A (!memchr (b, 0, 2));
  A (&b[2] == memchr (b, 0, sizeof b));

  A (!memchr (c, 0, 2));
  A (&c[3] == memchr (c, 0, 4));
  A (&c[3] == memchr (c, 0, sizeof a));
}

/* { dg-final { scan-tree-dump-not "abort" "release_ssa" } } */
