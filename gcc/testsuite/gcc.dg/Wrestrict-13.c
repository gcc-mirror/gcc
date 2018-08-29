/* PR tree-optimization/83519 - missing -Wrestrict on an overlapping
   strcpy to a non-member array
   { dg-do compile }
   { dg-options "-O2 -Wall -Wrestrict" }  */

extern char* stpcpy (char*, const char*);   // work around bug 82429

struct S { char a[17]; };

void f (struct S *p, const char *s)
{
  __builtin_strcpy (p->a, "0123456789abcdef");

  __builtin_strcpy (p->a, p->a + 4);    /* { dg-warning "\\\[-Wrestrict]" } */
}

char a[17];

void g (const char *s)
{
  __builtin_strcpy (a, "0123456789abcdef");

  __builtin_strcpy (a, a + 4);          /* { dg-warning "\\\[-Wrestrict]" } */
}

void h (const char *s)
{
   char a[17];

  __builtin_strcpy (a, "0123456789abcdef");

  __builtin_strcpy (a, a + 4);          /* { dg-warning "\\\[-Wrestrict]" } */

  extern void sink (void*);
  sink (a);
}
