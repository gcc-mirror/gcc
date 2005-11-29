/* Ensure that transformations of *printf are performed correctly
   regardless of -fexec-charset.  See PR 25120.  */

/* { dg-do compile } */
/* { dg-require-iconv "IBM1047" } */
/* { dg-options "-O2 -fexec-charset=IBM1047" } */
/* { dg-final { scan-assembler-not "printf" } } */
/* { dg-final { scan-assembler-not "fprintf" } } */
/* { dg-final { scan-assembler-not "sprintf" } } */

#include <stdio.h>

void foo (char *dst, const char *src)
{
  printf ("\n");
  printf ("hello world\n");
  printf ("%s", "\n");
  printf ("%s", "hello world\n");
  printf ("%c", '\n');
  printf ("%s\n", "hello world");
  printf ("%s\n", src);
  
  fprintf (stdout, "\n");
  fprintf (stdout, "hello world\n");
  fprintf (stdout, "%s", "\n");
  fprintf (stdout, "%s", "hello world\n");
  fprintf (stdout, "%c", '\n');
  fprintf (stdout, "%s", src);

  sprintf (dst, "hello world\n");
  sprintf (dst, "%s", src);
}
