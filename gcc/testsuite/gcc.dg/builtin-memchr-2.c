/* Verify that memchr calls with the address of a constant character
   are folded as expected even at -O0.
  { dg-do compile }
  { dg-options "-O0 -Wall -fdump-tree-gimple" } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

extern void* memchr (const void*, int, size_t);
extern int printf (const char*, ...);
extern void abort (void);

#define A(expr)							\
  ((expr)							\
   ? (void)0							\
   : (printf ("assertion failed on line %i: %s\n",		\
			__LINE__, #expr),			\
      abort ()))

const char nul = 0;
const char cha = 'a';

const struct
{
  char c;
} snul = { 0 },
  schb = { 'b' },
  sarr[] = {
  { 0 },
  { 'c' }
  };


void test_memchr_cst_char (void)
{
  A (&nul == memchr (&nul, 0, 1));
  A (!memchr (&nul, 'a', 1));

  A (&cha == memchr (&cha, 'a', 1));
  A (!memchr (&cha, 0, 1));

  A (&snul.c == memchr (&snul.c, 0, 1));
  A (!memchr (&snul.c, 'a', 1));

  A (&schb.c == memchr (&schb.c, 'b', 1));
  A (!memchr (&schb.c, 0, 1));

  A (&sarr[0].c == memchr (&sarr[0].c, 0, 1));
  A (!memchr (&sarr[0].c, 'a', 1));

  A (&sarr[1].c == memchr (&sarr[1].c, 'c', 1));
  A (!memchr (&sarr[1].c, 0, 1));
}

/* { dg-final { scan-tree-dump-not "abort" "gimple" } } */
