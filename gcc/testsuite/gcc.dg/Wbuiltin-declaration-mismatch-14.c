/* PR c/94040 - ICE on a call to an invalid redeclaration of strftime
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

struct tm;

size_t strftime (char *, size_t, int *, struct tm *);   // { dg-warning "-Wbuiltin-declaration-mismatch" }

size_t call_strftime (char *d, size_t n, int *f, struct tm *t)
{
  size_t r = 0;
  r += strftime (0, 0, 0, 0);
  r += strftime (d, 0, 0, 0);
  r += strftime (d, n, 0, 0);
  r += strftime (d, n, f, 0);
  r += strftime (d, n, f, t);
  return r;
}


char* strchr (char*, char*); // { dg-warning "-Wbuiltin-declaration-mismatch" }

// Verify that missing/extra qualifiers aren't diagnosed without -Wextra.

int strcmp (char*, char*);
int strncmp (volatile char*, volatile char*, size_t);

// Verify that a difference in pointers is diagnosed.

size_t strlen (const char**);
// { dg-warning "-Wbuiltin-declaration-mismatch" "pointer" { target *-*-* } .-1 }

 size_t strnlen (const char* const*, size_t);
// { dg-warning "-Wbuiltin-declaration-mismatch" "pointer" { target *-*-* } .-1 }


// Verify that calls to the compatibly-redeclared built-ins are treated
// as those to the built-ins and diagnosed.

int test_builtin_calls (size_t n)
{
  int r = 0;
  r += strcmp ((char*)0, "");               // { dg-warning "\\\[-Wnonnull]" }
  r += strcmp ("", (char*)0);               // { dg-warning "\\\[-Wnonnull]" }

  r += strncmp ((char*)0, "", n);           // { dg-warning "\\\[-Wnonnull]" }
  r += strncmp ("", (char*)0, n);           // { dg-warning "\\\[-Wnonnull]" }

  return r;
}


// Verify that calls to the incompatibly-redeclared built-ins are not
// treated as those to the built-ins by the middle-end.  It doesn't
// matter if the front-end diagnoses them but the middle-end should
// not because it shouldn't recognize them as built-ins.

#pragma GCC optimize "2"

size_t test_nonbuiltin_calls (char *s, int c)
{
  void *null = 0;

  char *r;
  r = strchr ((char*)null, s);
  r = strchr (r, (char*)null);
  *s = *r;   // use the result

  size_t n = 0;
  n += strftime (0, 0, 0, 0);
  n += strlen ((const char**)null);
  n += strnlen ((const char**)null, n);

  return n;
}
