/* This used to ICE due to a literal pool handling bug on s390x.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

static struct table { int x; } table[3];

int test (void)
{
  struct table *t;

  for (t = table; t < &table[3]; t++)
    asm volatile ("" : : : "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12");

  for (t = table; t < &table[3]; t++)
    if (t->x)
      return 1;

  return 0;
}

