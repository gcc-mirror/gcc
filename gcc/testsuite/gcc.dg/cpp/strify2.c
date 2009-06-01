/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "-std=c99 -pedantic-errors" } */

/* Tests a whole bunch of things are correctly stringified.  */

extern int strcmp (const char *, const char *);
extern int puts (const char *);
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

#define str(x) #x
#define xstr(x) str(x)
#define strvar(...) #__VA_ARGS__

#define glibc_str(x) glibc_str2 (w, x)
#define glibc_str2(w, x) #x
#define ver GLIBC_2.2

int main (int argc, char *argv[])
{
  str (\);		/* { dg-warning "valid string" "str(\\)" } */
  str (\\);		/* OK.  */
  str (\\\);		/* { dg-warning "valid string" "str(\\\\\\)" } */

  /* This also serves as a useful test of the value of __INCLUDE_LEVEL.  */
  if (strcmp (xstr (__INCLUDE_LEVEL__), "0"))
    err ("macro expansion");

  if (strcmp(str (__INCLUDE_LEVEL__), "__INCLUDE_LEVEL__"))
    err ("macro name");

  if (strcmp(str(), "") || strcmp(str( ), ""))
    err ("empty string");

  if (strcmp(str ("s\n"), "\"s\\n\""))
    err ("quoted string");

  if (strcmp (str (a € b), "a \200 b"))
    err ("unprintable char");

  if (strcmp (str (	a    b@ c   ), "a b@ c"))
    err ("internal whitespace");

  if (strcmp (str(a \n), "a \n"))
    err ("backslash token");

  if (strcmp (strvar (foo, bar), "foo, bar"))
    err ("variable arguments");

  if (strcmp (glibc_str (ver), "GLIBC_2.2"))
    err ("whitespace");

  return 0;
}
