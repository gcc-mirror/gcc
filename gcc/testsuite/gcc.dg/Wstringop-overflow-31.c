/* PR middle-end/93646 - confusing -Wstringop-truncation on strncat where
   -Wstringop-overflow is expected
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

extern __SIZE_TYPE__ strlen (const char*);
extern char* strncat (char*, const char*, __SIZE_TYPE__);


char a[4];


void f0 (char *d, const char *s)
{
  strncat (d, s, strlen (s));   // { dg-warning "specified bound depends on the length of the source argument" }
  /* { dg-message "function 'f0'.*inlined from 'f1'" "inlining stack" { target *-*-* } 0 }  */

  // Prevent f0 from being replaced by g0.
  *d = 'f';
}

void f1 (const char *s)
{
  f0 (a, s);
}


static void g0 (char *d, const char *s)
{
  strncat (d, s, strlen (s));   // { dg-warning "specified bound 3 equals source length" }
  /* { dg-message "function 'g0'.*inlined from 'g1'" "inlining stack" { target *-*-* } 0 }  */

  // Prevent g0 from being replaced by f0.
  *d = 'g';
}

void g1 (void)
{
  g0 (a, "123");
}
