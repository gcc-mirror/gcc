/* PR bootstrap/77676 - powerpc64 and powerpc64le stage2 bootstrap fail
   Test case from comment 6 on the bug.  */
/* { dg-do compile } */
/* { dg-options "-Wall -Werror" } */

struct A
{
  const char *a;
  int b;
  const char *c;
};

void bar (char *);

void
foo (struct A *p)
{
  char s[4096];
  const char *u = p->a;
  const char *t;
  while ((t = __builtin_strstr (u, "gcc/")))
    u = t + 4;

  /* Verfiy the following doesn't emit a warning.  */
  __builtin_sprintf (s, "%s:%i (%s)", u, p->b, p->c);
  bar (s);
}
