/* { dg-do run } */
/* { dg-options "-ansi" } */

extern void abort (void);

/* Basic tests for trigraph conversion.
   All of them are here, but not in all possible contexts.  *??/
/

??=include <stdio.h>

??=define TWELVE 1??/
2

static const char str??(??) = "0123456789??/n";

int
main(void)
??<
  unsigned char x = 5;

  if (sizeof str != TWELVE)
    abort ();

  /* Test ^=, the only multi-character token to come from trigraphs.  */
  x ??'= 3;
  if (x != 6)
    abort ();

  if ((5 ??! 3) != 7)
    abort ();

  return 0;
??>
