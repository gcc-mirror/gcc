/* { dg-do run } */

/* Test #line with and without macros for the line number.  */

#define L 90

#line 44
int i = __LINE__;

#line L
int j = __LINE__;

#line 14  /* N.B. the _next_ line is line 14.  */

int main(void)
{
  if (i != 44)
    abort ();
  if (j != 90)
    abort ();
  if (__LINE__ != 21)
    abort ();
  return 0;
}
