/* Copyright (C) 2003  Free Software Foundation.

   Test sprintf optimizations don't break anything and return the
   correct results.

   Written by Roger Sayle, June 22, 2003.  */

static char buffer[32];

extern void abort ();
typedef __SIZE_TYPE__ size_t;
extern int sprintf(char*, const char*, ...);
extern void *memset(void*, int, size_t);
extern int memcmp(const void*, const void*, size_t);

void test1()
{
  sprintf(buffer,"foo");
}

int test2()
{
  return sprintf(buffer,"foo");
}

void test3()
{
  sprintf(buffer,"%s","bar");
}

int test4()
{
  return sprintf(buffer,"%s","bar");
}

void test5(char *ptr)
{
  sprintf(buffer,"%s",ptr);
}


int main()
{
  memset (buffer, 'A', 32);
  test1 ();
  if (memcmp(buffer, "foo", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  if (test2 () != 3)
    abort ();
  if (memcmp(buffer, "foo", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  test3 ();
  if (memcmp(buffer, "bar", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  if (test4 () != 3)
    abort ();
  if (memcmp(buffer, "bar", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  test5 ("barf");
  if (memcmp(buffer, "barf", 5) || buffer[5] != 'A')
    abort ();

  return 0;
}

#ifdef __OPTIMIZE__
/* When optimizing, all the above cases should be transformed into
   something else.  So any remaining calls to the original function
   should abort.  */
__attribute__ ((noinline))
static int
sprintf (char *buf, const char *fmt, ...)
{
  abort ();
}
#endif

