/* PR middle-end/89934 - ICE on a call with fewer arguments to strncpy
   declared without prototype
   { dg-do compile }
   { dg-options "-std=gnu17 -O2 -Wall" } */

typedef __SIZE_TYPE__ size_t;

char *strncpy ();

char* f0 (char *s)
{
  return strncpy ();
}

char* f1 (char *s)
{
  return strncpy (s);
}

char* f2 (char *s)
{
  return strncpy (s, s + 1);   /* ICE here.  */
}

void f3 (char *s, size_t n, const char *t)
{
  strncpy (s, n, t);
  strncpy (n, s, t);
}

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" }
   { dg-prune-output "\\\[-Wint-conversion]" } */
