/* Copyright (C) 2000  Free Software Foundation  */
/* by Alexandre Oliva <aoliva@redhat.com> */

#include <stdlib.h>
#include <string.h>

char *list[] = { "*", "e" };

static int bar (const char *fmt) {
  return (strchr (fmt, '*') != 0);
}

static void foo () {
  int i;
  for (i = 0; i < sizeof (list) / sizeof (*list); i++) {
    const char *fmt = list[i];
    if (bar (fmt))
      continue;
    if (i == 0)
      abort ();
    else
      exit (0);
  }
}

int main () {
  foo ();
}
