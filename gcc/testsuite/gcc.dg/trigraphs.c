/* { dg-do run } */
/* { dg-options "-ansi" } */

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
  if (sizeof str != TWELVE)
    abort ();

  if ((5 ??' 3) != 6)
    abort ();

  if ((5 ??! 3) != 7)
    abort ();

  return 0;
??>
