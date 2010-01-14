/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling -fsel-sched-pipelining" } */

int
strictly_smaller_name (char *s, char *t)
{
  int ss, tt;
  while ((*s != '\0') || (*t != '\0'))
    {
      if (*s == '\0')
        ss = '*';
      else
        ss = *s++;
      if (*t != '\0')
        tt = *t;
      if (ss == tt)
        return 0;
    }
}

