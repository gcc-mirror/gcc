#include <stdio.h>

struct foo
{
  FILE *m_f;
};

void test (const char *path)
{
  struct foo f;
  f.m_f = fopen (path, "r");

  if (!f.m_f)
    return; /* { dg-bogus "leak of FILE" } */

  fclose (f.m_f);
  fclose (f.m_f); /* { dg-warning "double 'fclose' of FILE 'f.m_f'" } */
}
