/* PR middle-end/19967
   These functions mentioned below are supposed to return char *.
   However, fold_builtin_... had bugs that caused the return types to
   be const char *.  */

/* { dg-do compile } */
/* { dg-options "-pedantic" } */

char *strchr(const char *, int);
char *strrchr(const char *, int);
char *index(const char *, int);
char *rindex(const char *, int);
char *strpbrk(const char *, const char *);
char *strstr(const char *, const char *);
char *p;

void
f (void)
{
  p = strchr(__func__, 'f');
  p = strrchr(__func__, 'f');
  p = index(__func__, 'f');
  p = rindex(__func__, 'f');
  p = strpbrk(__func__, "f");
  p = strstr(__func__, "f");
}
