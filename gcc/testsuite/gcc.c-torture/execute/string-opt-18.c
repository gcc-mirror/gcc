/* Copyright (C) 2003  Free Software Foundation.

   Test equal pointer optimizations don't break anything.

   Written by Roger Sayle, July 14, 2003.  */

extern void abort ();
typedef __SIZE_TYPE__ size_t;

extern void *memcpy(void*, const void*, size_t);
extern void *mempcpy(void*, const void*, size_t);
extern void *memmove(void*, const void*, size_t);
extern char *strcpy(char*, const char*);
extern int memcmp(const void*, const void*, size_t);
extern int strcmp(const char*, const char*);
extern int strncmp(const char*, const char*, size_t);


void test1 (void *ptr)
{
  if (memcpy(ptr,ptr,8) != ptr)
    abort ();
}

void test2 (char *ptr)
{
  if (mempcpy(ptr,ptr,8) != ptr+8)
    abort ();
}

void test3 (void *ptr)
{
  if (memmove(ptr,ptr,8) != ptr)
    abort ();
}

void test4 (char *ptr)
{
  if (strcpy(ptr,ptr) != ptr)
    abort ();
}

void test5 (void *ptr)
{
  if (memcmp(ptr,ptr,8) != 0)
    abort ();
}

void test6 (const char *ptr)
{
  if (strcmp(ptr,ptr) != 0)
    abort ();
}

void test7 (const char *ptr)
{
  if (strncmp(ptr,ptr,8) != 0)
    abort ();
}


int main ()
{
  char buf[10];

  test1 (buf);
  test2 (buf);
  test3 (buf);
  test4 (buf);
  test5 (buf);
  test6 (buf);
  test7 (buf);

  return 0;
}

