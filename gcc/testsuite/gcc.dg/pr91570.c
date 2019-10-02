/* PR tree-optimization/91570 - ICE in get_range_strlen_dynamic on
   a conditional of two strings
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern char a[], b[];

/* Test case from comment #0 on the bug.  */

void comment_0 (int i)
{
  a[0] = 0;
  b[0] = '1';

  const char *p = i ? b : a;

  if (__builtin_snprintf (0, 0, "%s", p) < 4)
    __builtin_abort ();
}


/* Test case from comment #2 on the bug.  */

void comment_2 (char *s)
{
  char *t = __builtin_strrchr (s, '/');
  __builtin_strcat (s, ".SIF");
  t = t ? t : s;
  __builtin_printf ("%s", t);
}
