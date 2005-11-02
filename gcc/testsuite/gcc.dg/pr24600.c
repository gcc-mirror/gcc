/* This used to ICE on s390 due to a old-loop bug.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

char *strcpy (char *dest, const char *src);

void test (char *Line, int len)
{
  int z;

  for (z = 1; z <= len; z++)
    if (Line[z - 1])
      strcpy (Line + z + 1, Line);
}

