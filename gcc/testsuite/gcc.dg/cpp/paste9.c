/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do run } */
/* { dg-options "" } */

/* Apparently older preprocessors used to fail this test.  */

#include <string.h>

extern void abort (void);

#define S(str, args...) "  " str "\n", ##args

int
main()
{
  const char *s = S("foo");

  if (strchr (s, '\n') == NULL)
    abort ();

  return 0;
}
