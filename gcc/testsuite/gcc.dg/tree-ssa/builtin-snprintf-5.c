/*
  { dg-do compile }
  { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern int snprintf (char*, size_t, const char*, ...);

const char s0[] = "";
const char s1[] = "a";
const char s2[] = "ab";

extern char ax[];
extern const char* const ptr;

#define CAT(x, y)      x ## y
#define CONCAT(x, y)   CAT (x, y)
#define TEST           CONCAT (test_on_line_, __LINE__)

#define KEEP(expr) do {				\
    if ((expr))	{				\
      extern void TEST (void);			\
      TEST ();					\
    }						\
  } while (0)


void test_literal (int i)
{
  KEEP (0 < snprintf (0, 0, "%s", i ? "" : ax));
  KEEP (1 < snprintf (0, 0, "%s", i ? ax : "1"));
  KEEP (2 < snprintf (0, 0, "%s", i ? "12" : ptr));

  KEEP (1 > snprintf (0, 0, "%s", i ? "" : ax));
  KEEP (1 > snprintf (0, 0, "%s", i ? ax : "1"));
  KEEP (2 > snprintf (0, 0, "%s", i ? "12" : ptr));
}

void test_cststr (int i)
{
  KEEP (0 < snprintf (0, 0, "%s", i ? s0 : ax));
  KEEP (1 < snprintf (0, 0, "%s", i ? ax : s1));
  KEEP (2 < snprintf (0, 0, "%s", i ? s2 : ptr));

  KEEP (1 > snprintf (0, 0, "%s", i ? s0 : ax));
  KEEP (1 > snprintf (0, 0, "%s", i ? ax : s1));
  KEEP (2 > snprintf (0, 0, "%s", i ? s2 : ptr));
}

/* { dg-final { scan-tree-dump-times "test_on_line_" 12 "optimized" } } */
