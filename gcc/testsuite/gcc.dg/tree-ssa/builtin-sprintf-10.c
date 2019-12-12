/* PR tree-optimization/86853 - sprintf optimization for wide strings
   doesn't account for conversion failureâ€‹
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;
typedef __WINT_TYPE__  wint_t;

extern int snprintf (char*, size_t, const char*, ...);

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name) CAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to funcation named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr) \
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define KEEP(expr)				\
  if (expr)					\
    FAIL (made_in_true_branch);			\
  else						\
    FAIL (made_in_false_branch)


extern wchar_t wc;
extern wchar_t ws[];

const wchar_t ws3[] = L"12\xff";

/* Verify that the following calls are eliminated.  */

void elim_wide_char_call (void)
{
  ELIM (snprintf (0, 0, "%lc", (wint_t)L'\0'));
  ELIM (snprintf (0, 0, "%lc", (wint_t)L'1'));
  ELIM (snprintf (0, 0, "%lc", (wint_t)L'a'));
  ELIM (snprintf (0, 0, "%lc", (wint_t)ws3[0]));
  ELIM (snprintf (0, 0, "%lc", (wint_t)ws3[1]));
  ELIM (snprintf (0, 0, "%lc", (wint_t)ws3[3]));

  ELIM (snprintf (0, 0, "%C", (wint_t)L'\0'));
  ELIM (snprintf (0, 0, "%C", (wint_t)L'9'));
  ELIM (snprintf (0, 0, "%C", (wint_t)L'z'));
  ELIM (snprintf (0, 0, "%C", (wint_t)ws3[0]));
  ELIM (snprintf (0, 0, "%C", (wint_t)ws3[1]));
  ELIM (snprintf (0, 0, "%C", (wint_t)ws3[3]));

  /* Verify an unknown character value within the ASCII range.  */
  if (wc < 1 || 127 < wc)
    wc = 0;

  ELIM (snprintf (0, 0, "%C", (wint_t)wc));
  ELIM (snprintf (0, 0, "%C", (wint_t)wc));
}

void elim_wide_string_call (void)
{
  ELIM (snprintf (0, 0, "%ls", L""));
}


#line 1000

  /* Verify that the following calls are not eliminated.  */

void keep_wide_char_call (void)
{
  KEEP (snprintf (0, 0, "%lc", (wint_t)L'\xff'));
  KEEP (snprintf (0, 0, "%lc", (wint_t)L'\xffff'));
  KEEP (snprintf (0, 0, "%lc", (wint_t)wc));
  KEEP (snprintf (0, 0, "%lc", (wint_t)ws3[2]));

  KEEP (snprintf (0, 0, "%C", (wint_t)L'\xff'));
  KEEP (snprintf (0, 0, "%C", (wint_t)L'\xffff'));
  KEEP (snprintf (0, 0, "%C", (wint_t)wc));
  KEEP (snprintf (0, 0, "%C", (wint_t)ws3[2]));

  /* Verify an unknown character value outside the ASCII range
     (with 128 being the only one).  */
  if (wc < 32 || 128 < wc)
    wc = 32;

  KEEP (snprintf (0, 0, "%lc", (wint_t)wc));
  KEEP (snprintf (0, 0, "%C", (wint_t)wc));
}

void keep_wide_string_call (void)
{
  KEEP (snprintf (0, 0, "%ls", L"\xff"));
  KEEP (snprintf (0, 0, "%ls", L"\xffff"));
  KEEP (snprintf (0, 0, "%ls", ws));
  KEEP (snprintf (0, 0, "%ls", ws3));

  KEEP (snprintf (0, 0, "%S", L"\xff"));
  KEEP (snprintf (0, 0, "%S", L"\xffff"));
  KEEP (snprintf (0, 0, "%S", ws));
  KEEP (snprintf (0, 0, "%S", ws3));
}

/* { dg-final { scan-tree-dump-times "call_made_in_true_branch_not_eliminated" 0 "optimized" } }

   { dg-final { scan-tree-dump-times "call_made_in_true_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 18 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 18 "optimized" } } */
