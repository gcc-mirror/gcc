/* Copyright (C) 2000 Free Software Foundation */

/* make sure we don't get confused by various flavors of void */

/* Origin:  Jakub Jelinek  <jakub@redhat.com>
 *          Joel Sherrill <joel.sherrill@OARcorp.com>
 */

typedef void foo;
foo bar(void);
void baz(void)
{
  bar();
}

void volatile f();

int x()
{
  f();
}

