/* Source: PR 137.

   We would not warn about passing an enum, but would warn about
   passing a enum that was part of an array.  TYPE_MAIN_VARIANT was
   not used in the appropriate place in the warning code.  */

/* { dg-do compile } */
/* { dg-options -Wconversion } */

typedef enum { a } __attribute__((packed)) t;
void f(t x) {}

int main(void)
{
  t x[2], y;
  f(x[0]);			/* { dg-bogus "different width" } */
  f(y);				/* { dg-bogus "different width" } */
  return 0;
}

