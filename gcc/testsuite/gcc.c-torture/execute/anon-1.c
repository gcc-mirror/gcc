/* Copyright (C) 2001 Free Software Foundation, Inc.  */

/* Source: Neil Booth, 4 Nov 2001, derived from PR 2820 - field lookup in
   nested anonymous entities was broken.  */

void abort (void);

struct
{
  int x;
  struct
  {
    int a;
    union
    {
      int b;
    };
  };
} foo;

int
main(int argc, char *argv[])
{
  foo.b = 6;
  foo.a = 5;

  if (foo.b != 6)
    abort ();

  return 0;
}
