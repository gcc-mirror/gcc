/* Verify that memchr calls with a pointer to a constant character
   are folded as expected.
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-gimple" } */

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

const char nul = 0;
const char cha = 'a';

const char* const pnul = &nul;
const char* const pcha = &cha;

const struct
{
  char c;
} snul = { 0 },
  schb = { 'b' },
  sarr[] = {
  { 0 },
  { 'c' }
  };

const char* const psarr0c = &sarr[0].c;
const char* const psarr1c = &sarr[1].c;

void test_memchr_cst_char (void)
{
  A (&nul == memchr (&nul, 0, 1));
  A (!memchr (&nul, 'a', 1));

  A (&cha == memchr (&cha, 'a', 1));
  A (!memchr (&cha, 0, 1));

  A (&nul == memchr (pnul, 0, 1));
  A (!memchr (pnul, 'a', 1));

  A (&cha == memchr (pcha, 'a', 1));
  A (!memchr (pcha, 0, 1));

  A (&snul.c == memchr (&snul.c, 0, 1));
  A (!memchr (&snul.c, 'a', 1));

  A (&schb.c == memchr (&schb.c, 'b', 1));
  A (!memchr (&schb.c, 0, 1));

  A (&sarr[0].c == memchr (&sarr[0].c, 0, 1));
  A (!memchr (&sarr[0].c, 'a', 1));

  A (&sarr[1].c == memchr (&sarr[1].c, 'c', 1));
  A (!memchr (&sarr[1].c, 0, 1));

  A (&sarr[0].c == memchr (psarr0c, 0, 1));
  A (!memchr (psarr0c, 'a', 1));

  A (&sarr[1].c == memchr (psarr1c, 'c', 1));
  A (!memchr (psarr1c, 0, 1));
}

/* { dg-final { scan-tree-dump-not "abort" "gimple" } } */
