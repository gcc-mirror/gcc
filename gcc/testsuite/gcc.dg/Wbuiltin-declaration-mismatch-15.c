/* PR c/94040 - ICE on a call to an invalid redeclaration of strftime
   { dg-do compile }
   { dg-options "-Wall -Wextra" } */

typedef __SIZE_TYPE__ size_t;

struct tm;

size_t strftime (const char *, size_t, char *, struct tm *);
// { dg-warning "-Wbuiltin-declaration-mismatch" "arg 1" { target *-*-* } .-1 }

// Verify that missing/extra qualifiers are diagnosed with -Wextra.

int strcmp (char*, const char*);
// { dg-warning "-Wbuiltin-declaration-mismatch" "arg 1" { target *-*-* } .-1 }

int strncmp (const char*, volatile char*, size_t);
// { dg-warning "-Wbuiltin-declaration-mismatch" "arg 2" { target *-*-* } .-1 }

size_t strlen (char*);
// { dg-warning "-Wbuiltin-declaration-mismatch" "arg 1" { target *-*-* } .-1 }


// Verify that calls to built-ins declared with missing/extra qualifiers
// are still treated as those to built-ins by the front-end.

int test_builtin_calls_fe (size_t n)
{
  int r = 0;
  r += strcmp ((char*)0, "");               // { dg-warning "\\\[-Wnonnull]" }
  r += strcmp ("", (char*)0);               // { dg-warning "\\\[-Wnonnull]" }

  r += strncmp ((char*)0, "", n);           // { dg-warning "\\\[-Wnonnull]" }
  r += strncmp ("", (char*)0, n);           // { dg-warning "\\\[-Wnonnull]" }

  r += strlen ((char*)0);                   // { dg-warning "\\\[-Wnonnull]" }
  return r;
}


// Ditto but by the middle-end.

#pragma GCC optimize "2"

int test_builtin_calls_me (void)
{
  char *null1 = 0;
  char *null2 = null1;
  char *null3 = null2;

  int r = 0;
  r += strcmp (null1, "123");               // { dg-warning "\\\[-Wnonnull]" }
  r += strncmp ("2345", null2, 4);          // { dg-warning "\\\[-Wnonnull]" }
  r += strlen (null3);                      // { dg-warning "\\\[-Wnonnull]" }
  return r;
}
