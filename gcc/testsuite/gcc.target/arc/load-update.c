/* { dg-do compile } */
/* { dg-options "-Os" } */

/* This caused a segfault due to incorrect rtl pattern in some
   instructions.  */

int a, d;
char *b;

void fn1()
{
  char *e = 0;
  for (; d; ++a)
    {
      char c = b [0];
      *e++ = '.';
      *e++ = 4;
      *e++ = "0123456789abcdef" [c & 5];
    }
}
