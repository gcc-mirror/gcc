/* PR tree-optimization/90662 - strlen of a string in a vla plus offset
   not folded
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Wall -fdump-tree-gimple -fdump-tree-optimized" } */

#include "strlenopt.h"

typedef __INT16_TYPE__   int16_t;
typedef __INT32_TYPE__   int32_t;

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name, counter) \
  CAT (CAT (CAT (call_ ## name ##_on_line_, __LINE__), _), counter)

#define FAIL(name, counter) do {			\
    extern void FAILNAME (name, counter) (void);	\
    FAILNAME (name, counter)();				\
  } while (0)

/* Macro to emit a call to funcation named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr) \
  if (!(expr)) FAIL (in_true_branch_not_eliminated, __COUNTER__); else (void)0

#define ARGS(...) __VA_ARGS__

void sink (void*, ...);


#define T(Type, expect, init, p)			\
  do {							\
    Type vla[n];					\
    char *ptr = strcpy ((char*)vla, init);		\
    ELIM (expect == strlen (p));			\
    sink (ptr);						\
  } while (0)

void test_char_vla_local (int n)
{
  T (char, 0, "", vla);
  T (char, 0, "\0", vla);
  T (char, 1, "1", vla);
  T (char, 2, "12", vla);
  T (char, 3, "123", vla);

  T (char, 2, "123", vla + 1);
  T (char, 1, "123", &vla[2]);
  T (char, 0, "123", &vla[1] + 2);

  T (char, 2, "123", &vla[2] - 1);
  T (char, 3, "123", &vla[1] - 1);

  T (char, 0, "", ptr);
  T (char, 0, "\0", ptr);
  T (char, 1, "1", ptr);
  T (char, 2, "12", ptr);
  T (char, 3, "123", ptr);

  T (char, 2, "123", ptr + 1);
  T (char, 1, "123", &ptr[2]);
  T (char, 0, "123", &ptr[1] + 2);
}

void test_int16_vla_local (int n)
{
  T (int16_t, 0, "", (char*)vla);
  T (int16_t, 0, "\0", (char*)vla);
  T (int16_t, 1, "1", (char*)vla);
  T (int16_t, 2, "12", (char*)vla);
  T (int16_t, 3, "123", (char*)vla);

  T (int16_t, 2, "1234", (char*)(vla + 1));
  T (int16_t, 2, "123456", (char*)&vla[2]);
  T (int16_t, 1, "123456", (char*)&vla[2] + 1);
  T (int16_t, 0, "123456", (char*)&vla[2] + 2);
  T (int16_t, 0, "123456", (char*)(&vla[1] + 2));

  T (int16_t, 3, "123456", (char*)&vla[2] - 1);
  T (int16_t, 4, "123456", (char*)&vla[2] - 2);

  T (int16_t, 0, "", ptr);
  T (int16_t, 0, "\0", ptr);
  T (int16_t, 1, "1", ptr);
  T (int16_t, 2, "12", ptr);
  T (int16_t, 3, "123", ptr);

  T (int16_t, 2, "123", ptr + 1);
  T (int16_t, 1, "123", &ptr[2]);
  T (int16_t, 0, "123", &ptr[1] + 2);
}

void test_int_vla_local (int n)
{
  T (int16_t, 0, "", (char*)vla);
  T (int16_t, 0, "\0", (char*)vla);
  T (int16_t, 1, "1", (char*)vla);
  T (int16_t, 2, "12", (char*)vla);
  T (int16_t, 3, "123", (char*)vla);

  T (int16_t, 2, "1234", (char*)(vla + 1));
  T (int16_t, 2, "123456", (char*)&vla[2]);
  T (int16_t, 1, "123456", (char*)&vla[2] + 1);
  T (int16_t, 0, "123456", (char*)&vla[2] + 2);
  T (int16_t, 0, "123456", (char*)(&vla[1] + 2));

  T (int, 0, "", ptr);
  T (int, 0, "\0", ptr);
  T (int, 1, "1", ptr);
  T (int, 2, "12", ptr);
  T (int, 3, "123", ptr);

  T (int, 2, "123", ptr + 1);
  T (int, 1, "123", &ptr[2]);
  T (int, 0, "123", &ptr[1] + 2);
}


#undef T
#define T(Type, expect, parr, init, p)			\
  do {							\
    Type (*parray)[] = *ppa++;				\
    char *ptr = strcpy ((char*)parr, init);		\
    (void)&ptr;						\
    ELIM (expect == strlen (p));			\
  } while (0)

/* Have the function take a pointer to pointers to arrays so that each
   test case can use its own pointer to avoid interference between.  */

void test_char_array_ptr (char (**ppa)[])
{
  T (char, 0, *parray, "", *parray);
  T (char, 0, *parray, "", &(*parray)[0]);

  T (char, 1, *parray, "1", &(*parray)[0]);
  T (char, 0, *parray, "1", &(*parray)[1]);

  T (char, 2, *parray, "12", &(*parray)[0]);
  T (char, 1, *parray, "12", &(*parray)[1]);
  T (char, 0, *parray, "12", &(*parray)[2]);

  T (char, 3, *parray, "123", &(*parray)[0]);
  T (char, 2, *parray, "123", &(*parray)[1]);
  T (char, 1, *parray, "123", &(*parray)[2]);
  T (char, 0, *parray, "123", &(*parray)[3]);

  T (char, 3, *parray, "123", ptr);
  T (char, 2, *parray, "123", &ptr[1]);
  T (char, 1, *parray, "123", &ptr[2]);
  T (char, 0, *parray, "123", &ptr[3]);
}

void test_int16_array_ptr (int16_t (**ppa)[])
{
  T (int16_t, 0, *parray, "", (char*)*parray);
  T (int16_t, 0, *parray, "", (char*)&(*parray)[0]);

  T (int16_t, 1, *parray, "1", (char*)&(*parray)[0]);
  T (int16_t, 0, *parray, "12", (char*)&(*parray)[1]);

  T (int16_t, 2, *parray, "12", (char*)&(*parray)[0]);
  T (int16_t, 1, *parray, "12", (char*)&(*parray)[0] + 1);
  T (int16_t, 2, *parray, "1234", (char*)&(*parray)[1]);
  T (int16_t, 1, *parray, "1234", (char*)&(*parray)[1] + 1);
  T (int16_t, 0, *parray, "1234", (char*)&(*parray)[2]);

  T (int16_t, 6, *parray, "123456", (char*)&(*parray)[0]);
  T (int16_t, 5, *parray, "123456", (char*)&(*parray)[0] + 1);
  T (int16_t, 0, *parray, "123456", (char*)&(*parray)[0] + 6);
  T (int16_t, 4, *parray, "123456", (char*)&(*parray)[1]);
  T (int16_t, 3, *parray, "123456", (char*)&(*parray)[1] + 1);
  T (int16_t, 0, *parray, "123456", (char*)&(*parray)[1] + 4);
  T (int16_t, 2, *parray, "123456", (char*)&(*parray)[2]);
  T (int16_t, 1, *parray, "123456", (char*)&(*parray)[2] + 1);
  T (int16_t, 0, *parray, "123456", (char*)&(*parray)[2] + 2);
  T (int16_t, 0, *parray, "123456", (char*)&(*parray)[3]);

  T (int16_t, 3, *parray, "123", (char*)ptr);
  T (int16_t, 2, *parray, "123", (char*)&ptr[1]);
  T (int16_t, 1, *parray, "123", (char*)&ptr[2]);
  T (int16_t, 0, *parray, "123", (char*)&ptr[3]);
}

/* { dg-final { scan-tree-dump-times "strlen" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "not_eliminated" 0 "optimized" } } */
