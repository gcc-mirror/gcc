/* Test to verify that snprintf can determine the length of a dynamically
   constructed string argument and fold the result into a constant.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

char* strcpy (char * restrict, const char * restrict);
int sprintf (char * restrict, const char *restrict, ...);
int snprintf (char * restrict, size_t, const char *restrict, ...);


#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name, counter)						\
  CAT (CAT (CAT (call_ ## name ##_on_line_, __LINE__), _), counter)

#define FAIL(name, counter) do {			\
    extern void FAILNAME (name, counter) (void);	\
    FAILNAME (name, counter)();				\
  } while (0)

/* Macro to emit a call to funcation named
   call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr)							\
  if (!(expr)) FAIL (in_true_branch_not_eliminated, __COUNTER__); else (void)0

#define ARGS(...) __VA_ARGS__

#define T(expect, init, fmt, ...)			\
  do {							\
    char a[] = init;					\
    ELIM (expect == snprintf (0, 0, fmt, __VA_ARGS__));	\
  } while (0)

/* Exercise a non-const local char array initialized by a string literal.  */
void test_assign_string (void)
{
  T (0, "", "%s", a);
  T (1, "1", "%s", a);
  T (4, "1234", "%s", a);
  T (5, "123", "s=%s", a);
  T (5, "1234", "s=%s", a + 1);
  T (2, "1234", "s=%s", a + 4);
  T (5, "12345", "s=%s", &a[2]);
  T (5, "123456", "s=%.*s", 3, &a[2]);
}

/* Exercise a non-const local char array initialized by an initializer
   list.  */
void test_assign_init_list (void)
{
  T (0, ARGS ({ 0 }), "%s", a);
  T (1, ARGS ({ 1, 0 }), "%s", a);
  T (3, ARGS ({ [3] = 0, [1] = 2, [0] = 1, [2] = 3 }), "%s", a);
  T (3, ARGS ({ [3] = 0, [1] = 2, [0] = 1, [2] = 3, [4] = 0 }), "%s", a);
  T (4, ARGS ({ 1, 2, 3, 4, 0 }), "%s", a);
  T (5, ARGS ({ 1, 2, 3, 0 }), "s=%s", a);
  T (5, ARGS ({ 1, 2, 3, 4, 0 }), "s=%s", a + 1);
  T (2, ARGS ({ 1, 2, 3, 4, 0 }), "s=%s", a + 4);
  T (5, ARGS ({ 1, 2, 3, 4, 5, 0 }), "s=%s", &a[2]);
  T (5, ARGS ({ 1, 2, 3, 4, 5, 6, 0 }), "s=%.*s", 3, &a[2]);
}

#if __x86_64__

/* Enabled only on x86_64 to work around PR 83543.  */

#undef T
#define T(expect, init, fmt, ...)			\
  do {							\
    struct { int n; char a[sizeof init]; }		\
    s = { sizeof init, init };				\
    ELIM (expect == snprintf (0, 0, fmt, __VA_ARGS__));	\
  } while (0)

/* Exercise a non-const local struct initialized by an initializer
   list.  */
void test_assign_aggregate (void)
{
  T (0, "", "%s", s.a);
  T (1, "1", "%s", s.a);
  T (4, "1234", "%s", s.a);
  T (5, "123", "s=%s", s.a);
  T (5, "1234", "s=%s", s.a + 1);
  T (2, "1234", "s=%s", s.a + 4);
  T (5, "12345", "s=%s", &s.a[2]);
  T (5, "123456", "s=%.*s", 3, &s.a[2]);
}

/* { dg-final { scan-tree-dump-times "Function test_assign_aggregate" 1 "optimized" { xfail { { ! { i?86-*-* x86_64-*-* } } || { ilp32 } } } } } */

#endif   /* x86_64 */

#undef T
#define T(expect, init, fmt, ...)			\
  do {							\
    char a[sizeof init];				\
    strcpy (a, init);					\
    ELIM (expect == snprintf (0, 0, fmt, __VA_ARGS__));	\
  } while (0)

/* Exercise a local char array initialized by a call to strcpy.  */
void test_local_strcpy (void)
{
  T (0, "", "%s", a);
  T (1, "1", "%s", a);
  T (2, "12", "%s", a);
  T (3, "123", "%s", a);
  T (4, "1234", "%s", a);
  T (5, "123", "s=%s", a);
  T (5, "1234", "s=%s", a + 1);
  T (2, "1234", "s=%s", a + 4);
  T (5, "12345", "s=%s", &a[2]);
  T (5, "123456", "s=%.*s", 3, &a[2]);
}

#undef T
#define T(expect, init, fmt, ...)			\
  do {							\
    char a[n];						\
    strcpy (a, init);					\
    ELIM (expect == snprintf (0, 0, fmt, __VA_ARGS__));	\
  } while (0)

/* Exercise a VLA initialized by a call to strcpy.  */
void test_vla_strcpy (unsigned n)
{
  T (0, "", "%s", a);
  T (1, "1", "%s", a);
  T (2, "12", "%s", a);
  T (3, "123", "%s", a);
  T (4, "1234", "%s", a);
  T (5, "123", "s=%s", a);
  T (5, "1234", "s=%s", a + 1);
  T (2, "1234", "s=%s", a + 4);
  T (5, "12345", "s=%s", &a[2]);
  T (5, "123456", "s=%.*s", 3, &a[2]);
}

/* { dg-final { scan-tree-dump-times "printf" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "strlen" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "not_eliminated" 0 "optimized" } } */
