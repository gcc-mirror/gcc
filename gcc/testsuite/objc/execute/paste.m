/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "" } */

#define str(x) #x
#define xstr(x) str(x)
#define glue(x, y) x ## y
extern int strcmp (const char *, const char *);
extern int puts (const char *);
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

int
main ()
{
  /* Test Objective C names.  */
  if (strcmp (xstr (glue (@, ident)), "@ident"))
    err ("Objective C names");
  return 0;
}
