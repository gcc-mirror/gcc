/* Test to verify that snprintf can determine the correct range
   of lengths of string arguments based on the results of prior
   calls to strlen.
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

void abort (void);
size_t strlen (const char *);
int snprintf (char * restrict, size_t, const char *restrict, ...);

void one_str_exact (const char *str)
{
  if (1 == strlen (str))
    if (1 != snprintf (0, 0, "%s", str))
      abort ();
}

void two_str_exact (const char *s1, const char *s2)
{
  if (1 == strlen (s1) && 2 == strlen (s2))
    if (3 != snprintf (0, 0, "%s%s", s1, s2))
      abort ();
}

void one_str_maxlen (const char *str)
{
  if (2 >= strlen (str))
    if (2 < snprintf (0, 0, "%s", str))
      abort ();
}

void two_str_maxlen (const char *s1, const char *s2)
{
  if (2 >= strlen (s1) && 3 >= strlen (s2))
    if (5 < snprintf (0, 0, "%s%s", s1, s2))
      abort ();
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
