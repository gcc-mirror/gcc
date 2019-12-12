/* Verify that strlen() calls with constant conditional expressions are
   eliminated as expected.

   { dg-do compile }
   { dg-options "-O1 -fdump-tree-optimized" }  */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char*);


#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macros to emit a call to funcation named
     call_failed_to_be_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr) \
  if ((expr)) FAIL (test_not_eliminated); else (void)0

extern char a3[3];
extern char a7[7];

struct MemArrays { char a[7], b[9]; };

struct MemArrays ma;

void test_elim_condexpr (int i)
{
  ELIM (6 < strlen (i ? "" : "123456"));
  ELIM (6 < strlen (i ? "123456" : ""));

  ELIM (4 < strlen (i < 1 ? "a" : i == 1 ? "ab" : "abc"));

  ELIM (3 < strlen (i ? "" : a3));
  ELIM (3 < strlen (i ? a3 : "1"));

  ELIM (6 < strlen (i ? "12" : a7));
  ELIM (6 < strlen (i ? a7 : "123"));

  ELIM (6 < strlen (i ? "1234" : a7));
  ELIM (7 < strlen (i ? a7 : "1234567"));

  ELIM (3 < strlen (i < 1 ? "a" : i == 1 ? "ab" : a3));
  ELIM (3 < strlen (i < 1 ? "a" : i == 1 ? a3 : "abc"));
  ELIM (3 < strlen (i < 1 ? a3 : i == 1 ? "a" : "abc"));

  ELIM (6 < strlen (i < 1 ? "a" : i == 1 ? "ab" : a7));
  ELIM (6 < strlen (i < 1 ? "a" : i == 1 ? a7 : "abc"));
  ELIM (6 < strlen (i < 1 ? a7 : i == 1 ? "a" : "abc"));

  ELIM (6 < strlen (i < 1 ? "a" : i == 1 ? a7 : a3));
  ELIM (6 < strlen (i < 1 ? a7 : i == 1 ? "a" : a3));

  {
    enum { maxlen = sizeof ma - 1 };
    ELIM (maxlen < strlen (ma.a));
  }

  {
    enum { maxlen = sizeof ma - __builtin_offsetof (struct MemArrays, b) - 1 };
    ELIM (maxlen < strlen (ma.b));
  }
}

/* { dg-final { scan-tree-dump-times "test_not_eliminated_" 0 "optimized" } } */
