/* PR middle-end/91582 - missing heap overflow detection for strcpy

   The -Warray-bounds instances here probably should be replaced by
   -Wstringop-overflow when it detects these overflows (see also
   the xfails in Wstringop-overflow-25.c).

   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-overflow -ftrack-macro-expansion=0" } */

#include "range.h"

#define INT_MAX     __INT_MAX__
#define INT_MIN     (-INT_MAX - 1)

#define ATTR(...)   __attribute__ ((__VA_ARGS__))
#define NOIPA       ATTR (noipa)

extern void* malloc (size_t);
extern char* strcpy (char*, const char*);

void sink (void*);

#define S36 "0123456789abcdefghijklmnopqrstuvwxyz"
#define S(N) (S36 + sizeof S36 - N - 1)

struct Flex
{
  char n, ax[];
};

extern struct Flex fx;
struct Flex f1 = { 1, { 1 } };
struct Flex f2 = { 2, { 1, 2 } };
struct Flex f3 = { 3, { 1, 2, 3 } };

#define T(src, f) do {				\
    char *s = src;				\
    char *d = f.ax;				\
    strcpy (d, s);				\
    sink (&f);					\
  } while (0)

NOIPA void test_strcpy_flexarray (void)
{
  T (S (0), fx);                // { dg-bogus "\\\[-Warray-bounds" "pr92815" }
  T (S (9), fx);                // { dg-bogus "\\\[-Warray-bounds" "pr92815" }

  T (S (0), f1);
  T (S (1), f1);                // { dg-warning "\\\[-Warray-bounds" }

  T (S (0), f2);
  T (S (1), f2);
  T (S (2), f2);                // { dg-warning "\\\[-Warray-bounds" }

  T (S (0), f3);
  T (S (2), f3);
  T (S (3), f3);                // { dg-warning "\\\[-Warray-bounds" }
  T (S (9), f3);                // { dg-warning "\\\[-Warray-bounds" }
}

#undef T
#define T(T, src, n) do {			\
    char *s = src;				\
    typedef struct { T n, ax[]; } Flex;		\
    Flex *p = (Flex*)malloc (sizeof *p + n);	\
    char *d = (char*)p->ax;			\
    strcpy (d, s);				\
    sink (p);					\
  } while (0)

NOIPA void test_strcpy_malloc_flexarray (void)
{
  size_t r_0_1 = UR (0, 1);

  T (char, S (0), r_0_1);
  T (char, S (1), r_0_1);       // { dg-warning "\\\[-Warray-bounds" }

  size_t r_1_2 = UR (1, 2);

  T (char, S (0), r_1_2);
  T (char, S (1), r_1_2);
  T (char, S (2), r_1_2);       // { dg-warning "\\\[-Warray-bounds" }

  size_t r_2_3 = UR (2, 3);

  T (char, S (0), r_2_3);
  T (char, S (2), r_2_3);
  T (char, S (3), r_2_3);       // { dg-warning "\\\[-Warray-bounds" }
  T (char, S (9), r_2_3);       // { dg-warning "\\\[-Warray-bounds" }
}
