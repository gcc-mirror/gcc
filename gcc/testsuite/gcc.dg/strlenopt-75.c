/* PR tree-optimization/91294 - strlen result of a conditional with
   an offset
   { dg-do run }
   { dg-options "-O2 -Wall" } */

#include "strlenopt.h"

#define NOIPA __attribute__ ((noclone, noinline, noipa))

int i = 0;

const char s[] = "1234567";

char a[32];

/* Exercise a memcpy overwriting a destination string of known length
   with a source argument involving a conditional expression with strings
   of unqual lengths, with the selected one being the longer of the two
   and resulting in no change to the length of the overwritten destination
   string.  */
NOIPA void test_memcpy_same_length ()
{
  memcpy (a, "123456789a", 11);
  memcpy (a + 6, i ? "78\0" : "789\0", 4);
  if (strlen (a) != 9)
    abort ();
}

/* Same as above but with strcpy/strcat.  */

NOIPA void test_strcpy_strcat_same_length ()
{
  strcpy (a, "12345678");
  strcat (a, "9a");
  memcpy (a + 6, i ? "78\0" : "789\0", 4);
  if (strlen (a) != 9)
    abort ();
}

/* Same as above but using a memcpy of a power-of-two size that gets
   (on some targets) transformed into a single MEM_REF assignment.  */

NOIPA void test_assign_same_length ()
{
  memcpy (a, s, 8);
  memcpy (a + 5, i ? "67\0" : "678\0", 4);
  if (strlen (a) != 8)
    abort ();
}

/* Same as above but resulting in increasing the length of the destination
   string.  */

NOIPA void test_memcpy_lengthen ()
{
  memcpy (a, "123456789a", 11);
  memcpy (a + 8, i ? "9a\0" : "9ab\0", 4);
  if (strlen (a) != 11)
    abort ();
}

NOIPA void test_strcpy_strcat_lengthen ()
{
  strcpy (a, "12345678");
  strcat (a, "9a");
  memcpy (a + 8, i ? "9a\0" : "9ab\0", 4);
  if (strlen (a) != 11)
    abort ();
}

NOIPA void test_assign_lengthen ()
{
  memcpy (a, s, 8);
  memcpy (a + 6, i ? "78\0" : "789\0", 4);
  if (strlen (a) != 9)
    abort ();
}

NOIPA void test_memcpy_shorten ()
{
  memcpy (a, "123456789a", 11);
  memcpy (a + 6, i ? "789\0" : "78\0", 4);
  if (strlen (a) != 8)
    abort ();
}

NOIPA void test_strcpy_strcat_shorten ()
{
  strcpy (a, "12345678");
  strcat (a, "9a");
  memcpy (a + 6, i ? "789\0" : "78\0", 4);
  if (strlen (a) != 8)
    abort ();
}

NOIPA void test_assign_shorten ()
{
  memcpy (a, s, 8);
  memcpy (a + 6, i ? "789\0" : "78\0", 4);
  if (strlen (a) != 8)
    abort ();
}


int main (void)
{
  test_memcpy_same_length ();
  test_strcpy_strcat_same_length ();
  test_assign_same_length ();

  test_memcpy_lengthen ();
  test_strcpy_strcat_lengthen ();
  test_assign_lengthen ();

  test_memcpy_shorten ();
  test_strcpy_strcat_shorten ();
  test_assign_shorten ();
}
