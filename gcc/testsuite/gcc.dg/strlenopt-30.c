/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-strlen" } */

__attribute__((no_icf))
_Bool f1(char *s)
{
  return __builtin_strstr (s, "hello") == s;
}

__attribute__((no_icf))
_Bool f2(char *s)
{
  return s == __builtin_strstr (s, "hello");
}

__attribute__((no_icf))
_Bool f3(char *s)
{
  return s != __builtin_strstr (s, "hello");
}

__attribute__((no_icf))
_Bool f4()
{
  char *foo_f4(void);
  char *t1 = foo_f4();
  char *t2 = __builtin_strstr (t1, "hello");
  _Bool t3 = t2 == t1;
  return t3;
}

__attribute__((no_icf))
void f5(char *s)
{
  char *t1 = __builtin_strstr (s, "hello");
  void foo_f5(void);
  if (t1 != s)
    foo_f5();
}

/* Do not perform transform, since strlen (t)
   is unknown.  */

__attribute__((no_icf))
_Bool f6(char *s, char *t)
{
  return __builtin_strstr (s, t) == s;
}

/* Do not perform transform in this case, since
   t1 doesn't have single use.  */

__attribute__((no_icf))
_Bool f7(char *s)
{
  void foo_f7(char *);

  char *t1 = __builtin_strstr (s, "hello");
  foo_f7 (t1);
  return (t1 == s);
}

/* { dg-final { scan-tree-dump-times "__builtin_strncmp" 5 "strlen1" } } */
