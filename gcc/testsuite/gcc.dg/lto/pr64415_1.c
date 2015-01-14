/* { dg-options "-g" } */

extern int n;

void bar(char *, int);

inline void bar(char *s, int i)
{
  char *p = s;

#ifdef V1
  if (i)
#else
  if (n)
#endif
    *s = 0;
}
