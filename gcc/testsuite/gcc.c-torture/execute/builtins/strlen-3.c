/* Copyright (C) 2004  Free Software Foundation.

   Test strlen on const variables initialized to string literals.

   Written by Jakub Jelinek, 9/14/2004.  */

extern void abort (void);
extern __SIZE_TYPE__ strlen (const char *);
extern char *strcpy (char *, const char *);
const char bar[] = "Hello, World!";
const char baz[] = "hello, world?";
const char larger[20] = "short string";
extern volatile int inside_main;

int l1 = 1;
int x = 6;

void
main_test(void)
{
#ifdef __OPTIMIZE__
  const char *foo;
  int i;
#endif

  if (strlen (bar) != 13)
    abort ();

  if (strlen (bar + 3) != 10)
    abort ();

  if (strlen (&bar[6]) != 7)
    abort ();

  if (strlen (bar + (x++ & 7)) != 7)
    abort ();
  if (x != 7)
    abort ();

#ifdef __OPTIMIZE__
  foo = bar;
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	foo = "HELLO, WORLD!";
      else if (i == l1)
	foo = bar;
      else if (i == l1 + 1)
	foo = "hello, world!";
      else
	foo = baz;
    }
  if (strlen (foo) != 13)
    abort ();
#endif

  if (strlen (larger) != 12)
    abort ();
  if (strlen (&larger[10]) != 2)
    abort ();

  inside_main = 0;
  /* This will result in strlen call, because larger
     array is bigger than its initializer.  */
  if (strlen (larger + (x++ & 7)) != 5)
    abort ();
  if (x != 8)
    abort ();
  inside_main = 1;
}
