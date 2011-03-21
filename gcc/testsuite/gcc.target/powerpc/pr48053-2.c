/* Test for ICE arising from VSX code generation.  */
/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=power7" } */
/* { dg-require-effective-target powerpc_vsx_ok } */

struct timeval
{
  long tv_sec;
  long tv_usec;
};

extern char *bar (struct timeval *);
int *error;

void
foo (void *ptr)
{
  struct timeval tm;
  long n1, n2;

  if (!ptr)
    {
      *error = 1;
      n1 = -1;
      n2 = -1;
    }
  else
    {
      n1 = 0;
      n2 = *error;
    }

  tm.tv_sec = n1;
  tm.tv_usec = n2;

  if (*error)
    bar (&tm);
}
