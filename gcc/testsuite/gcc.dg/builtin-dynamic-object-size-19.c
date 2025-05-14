/* PR tree-optimization/88372 - alloc_size attribute is ignored
   on function pointers { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

#define __builtin_object_size __builtin_dynamic_object_size
#include "builtin-object-size-18.c"

typedef __SIZE_TYPE__ size_t;

#define ATTR(...) __attribute__ ((__VA_ARGS__))
#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name) CAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to function named
   call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr)							\
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

void sink (void*);

#define T(alloc, n) do {			\
    void *p = alloc;				\
    sink (p);					\
    ELIM (n == __builtin_object_size (p, 0));	\
    ELIM (n == __builtin_object_size (p, 1));	\
    ELIM (n == __builtin_object_size (p, 2));	\
    ELIM (n == __builtin_object_size (p, 3));	\
  } while (0)


ATTR (alloc_size (1)) void* (*alloc_1_x)(size_t, size_t);
ATTR (alloc_size (2)) void* (*alloc_x_2)(size_t, size_t);

/* Verify that things work when attribute alloc_size is applied
   to a typedef that is then used to declared a pointer.  */
typedef ATTR (alloc_size (1, 2)) void* (alloc_1_2_t)(size_t, size_t);

void test_alloc_ptr (alloc_1_2_t *alloc_1_2)
{
  T (alloc_1_x (0, 0), 0);
  T (alloc_1_x (1, 0), 1);
  T (alloc_1_x (3, 0), 3);
  T (alloc_1_x (9, 5), 9);

  T (alloc_x_2 (0, 0), 0);
  T (alloc_x_2 (1, 0), 0);
  T (alloc_x_2 (0, 1), 1);
  T (alloc_x_2 (9, 5), 5);

  T (alloc_1_2 (0, 0), 0);
  T (alloc_1_2 (1, 0), 0);
  T (alloc_1_2 (0, 1), 0);
  T (alloc_1_2 (9, 5), 45);
}

/* Verify that object size is detected even in indirect calls via
   function pointers to built-in allocation functions, even without
   explicit use of attribute alloc_size on the pointers.  */

typedef void *(allocfn_1) (size_t);
typedef void *(allocfn_1_2) (size_t, size_t);

static inline void *
call_alloc (allocfn_1 *fn1, allocfn_1_2 *fn2, size_t n1, size_t n2)
{
  return fn1 ? fn1 (n1) : fn2 (n1, n2);
}

static inline void *
call_malloc (size_t n)
{
  return call_alloc (__builtin_malloc, 0, n, 0);
}

static inline void *
call_calloc (size_t n1, size_t n2)
{
  return call_alloc (0, __builtin_calloc, n1, n2);
}

void test_builtin_ptr (void)
{
  T (call_malloc (0), 0);
  T (call_malloc (1), 1);
  T (call_malloc (9), 9);

  T (call_calloc (0, 0), 0);
  T (call_calloc (0, 1), 0);
  T (call_calloc (1, 0), 0);
  T (call_calloc (1, 1), 1);
  T (call_calloc (1, 3), 3);
  T (call_calloc (2, 3), 6);
}

/* { dg-final { scan-tree-dump-not "not_eliminated" "optimized" } } */
