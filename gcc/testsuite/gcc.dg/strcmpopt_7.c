/* PR tree-optimization/92501 - strncmp with constant unterminated arrays
   not folded
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-forwprop1" } */

/* Unterminated arrays of the size encoded in name.  */
const char a1[] = { '1' };
const char a12[] = { '1', '2' };
const char a112[] = { '1', '1', '2' };
const char a123[] = { '1', '2', '3' };

/* Nul-terminated strings of the length encoded in name.  */
const char s[] = "";
const char s1[] = "1";
const char s12[] = "12";
const char s112[] = "112";
const char s123[] = "123";

extern void failure_on_line (int);

/* Verify that the test in 'if (EQL strncmp (S, T, N))' is folded.  */
#define T(eql, s, t, n) do {			\
    if (!(eql __builtin_strncmp (s, t, n)))	\
      failure_on_line (__LINE__);		\
  } while (0)


void test (void)
{
  /* Mixed array and string.  */
  T (0 ==, a1, "", 0);
  T (0 ==, a1, s, 0);
  T (0 !=, a1, "", 1);
  T (0 !=, a1, s, 1);

  /* The following two are safe to fold because while strncmp compares
     at most N bytes it doesn't compare any bytes past the first nul.  */
  T (0 !=, a1, "", 9);
  T (0 !=, a1, s, 9);

  T (0 ==, a1, "1", 0);
  T (0 ==, a1, s1, 0);
  T (0 ==, a1, "1", 1);
  T (0 ==, a1, s1, 1);
  T (0 ==, a1, "12", 1);
  T (0 ==, a1, s12, 1);

     /* As above, the following three are also safe to fold.  */
  T (0 !=, a1, s12 + 1, 1);
  T (0 !=, a1, s12 + 1, 2);
  T (0 !=, a1, s12 + 1, 9);

  T (0 ==, a12, s, 0);
  T (0 ==, a12, "", 0);
  T (0 ==, a12, s1, 0);
  T (0 ==, a12, "1", 0);
  T (0 ==, a12, s1, 1);
  T (0 ==, a12, "1", 1);
  T (0 !=, a12, s1, 2);
  T (0 !=, a12, "1", 2);
  T (0 ==, a12, s12, 0);
  T (0 ==, a12, "12", 0);
  T (0 ==, a12, s12, 1);
  T (0 ==, a12, "12", 1);
  T (0 ==, a12, s12, 2);
  T (0 ==, a12, "12", 2);
  T (0 ==, a12, s123, 2);
  T (0 ==, a12, "123", 2);

  T (0 ==, a12 + 0, s123 + 1, 0);
  T (0 !=, a12 + 0, s123 + 1, 1);
  T (0 !=, a12 + 0, s123 + 1, 2);
  T (0 ==, a12 + 1, s123 + 0, 0);
  T (0 !=, a12 + 1, s123 + 0, 1);
  T (0 !=, a12 + 1, s123 + 0, 2);
  T (0 ==, a12 + 1, s123 + 1, 1);
  T (0 !=, a12 + 1, s123 + 2, 1);
  T (0 !=, a12 + 1, s123 + 3, 1);

  T (0 ==, a12 + 1, "123" + 1, 1);
  T (0 !=, a12 + 1, "123" + 2, 1);
  T (0 !=, a12 + 1, "123" + 3, 1);
  T (0 !=, a12 + 1, "123" + 3, 9);

  /* Both arguments arrays.  */
  T (0 ==, a112 + 0, a1, 1);
  T (0 ==, a112 + 1, a1, 1);
  T (0 !=, a112 + 2, a1, 1);

  T (0 ==, a1, a112 + 0, 1);
  T (0 ==, a1, a112 + 1, 1);
  T (0 !=, a1, a112 + 2, 1);

  T (0 ==, a112 + 0, a12, 0);
  T (0 ==, a112 + 0, a12, 1);
  T (0 !=, a112 + 0, a12, 2);

  T (0 ==, a112 + 1, a12, 2);
  T (0 !=, a112 + 1, a12 + 1, 1);
  T (0 ==, a112 + 2, a12 + 1, 1);

  /* Mixed array and string.  */
  T (0 ==, s112 + 0, a12, 0);
  T (0 ==, s112 + 0, a12, 1);
  T (0 !=, s112 + 0, a12, 2);

  T (0 ==, s112 + 1, a12, 0);
  T (0 ==, s112 + 1, a12, 1);
  T (0 ==, s112 + 1, a12, 2);
  T (0 !=, s112 + 2, a12, 2);

  T (0 ==, a112 + 0, s1, 1);
  T (0 ==, a112 + 1, s1, 1);
  T (0 !=, a112 + 2, s1, 1);
}

/* { dg-final { scan-tree-dump-not "strcmp" "forwprop1" } }
   { dg-final { scan-tree-dump-not "strncmp" "forwprop1" } }
   { dg-final { scan-tree-dump-not "failure_on_line_" "forwprop1" } } */
