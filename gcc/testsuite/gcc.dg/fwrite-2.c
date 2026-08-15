/* Check that folding fwrite of a single byte to fputc keeps the output the
   same, including the bytes that are not folded, and that a side effect in the
   stream argument is still evaluated exactly once.  */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-unused-result" } */

#include <stdio.h>

extern void abort (void);

const char s[] = "abcd";

int
main (void)
{
  FILE *streams[] = { stdout, NULL }, **p = streams;

  fwrite (s, 1, 1, stdout);
  fwrite (s + 1, 1, 1, stdout);
  fwrite (s + 2, 1, 2, stdout);
  fwrite (s, 2, 1, stdout);

  /* The folded call must still advance P exactly once.  */
  fwrite (s, 1, 1, *p++);
  if (p != streams + 1 || *p != NULL)
    abort ();

  fflush (stdout);
  return 0;
}

/* { dg-output "abcdaba" } */
