/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* { dg-do run } */

/* Tests we stringify without inserting a space.  GCC 2.95.x and
   earlier would insert a bogus space before bar in the string, simply
   because a space was there in the invocation.

   Neil Booth, 24 Sep 2001.  */

extern int strcmp (const char *, const char *);
#if DEBUG
extern int puts (const char *);
#else
#define puts(X)
#endif
extern void abort (void);
#define err(str) do { puts(str); abort(); } while (0)

#define str(x) #x
#define xstr(x) str(x)
#define glibc_hack(x, y) x@y

int main (int argc, char *argv[])
{
  /* The space before "bar" here is vital.  */
  char a[] = xstr(glibc_hack(foo, bar));

  if (strcmp (a, "foo@bar"))
    err ("stringification without spaces");

  return 0;
}
